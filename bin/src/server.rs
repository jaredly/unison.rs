use futures::{FutureExt, StreamExt};
use rust_embed::RustEmbed;
use std::collections::HashMap;
use std::sync::Arc;
use std::thread;
use tokio::sync::{mpsc, watch, RwLock};
use warp::filters::ws::{Message, WebSocket};
use warp::Filter;

fn as_filter<'a, T: RustEmbed>(
) -> impl Filter<Extract = (http::Response<std::borrow::Cow<'a, [u8]>>,), Error = warp::Rejection> + Copy
{
    use futures::future::{ready, Either};
    use http::{Response, StatusCode};
    use warp::filters::path;
    use warp::reject::not_found;
    warp::get()
        .and(path::tail())
        .and_then(|tail: path::Tail| match T::get(tail.as_str()) {
            None => Either::Right(ready(Err(not_found()))),
            Some(value) => Either::Left(ready(Ok(Response::builder()
                .header("Content-type", mime_for_ext(tail.as_str()))
                .status(StatusCode::OK)
                .body(value)
                .unwrap()))),
        })
}

fn index_filter<'a, T: RustEmbed>(
) -> impl Filter<Extract = (http::Response<std::borrow::Cow<'a, [u8]>>,), Error = warp::Rejection> + Copy
{
    use futures::future::{ready, Either};
    use http::{Response, StatusCode};
    use warp::filters::path;
    use warp::reject::not_found;
    warp::get()
        .and(path::end())
        .and_then(|| match T::get("index.html") {
            None => Either::Right(ready(Err(not_found()))),
            Some(value) => Either::Left(ready(Ok(Response::builder()
                .header("Content-type", "text/html")
                .status(StatusCode::OK)
                .body(value)
                .unwrap()))),
        })
}

type Receiver = watch::Receiver<Message>;

#[derive(Default)]
struct Pool {
    listeners: HashMap<usize, mpsc::UnboundedSender<Result<Message, warp::Error>>>,
    next_id: usize,
}

impl Pool {
    fn push(&mut self, tx: mpsc::UnboundedSender<Result<Message, warp::Error>>) -> usize {
        let n = self.next_id;
        self.next_id += 1;
        self.listeners.insert(n, tx);
        n
    }

    fn drop(&mut self, id: usize) {
        self.listeners.remove(&id);
    }

    fn broadcast(&self, message: Message) {
        for l in self.listeners.values() {
            if let Err(_) = l.send(Ok(message.clone())) {
                // it's fine
            }
        }
    }
}

type PoolRef = Arc<RwLock<Pool>>;

async fn add(pool: PoolRef, ws: WebSocket, codebase: Arc<RwLock<crate::branch::Codebase>>) {
    let (user_ws_tx, mut user_ws_rx) = ws.split();

    // Use an unbounded channel to handle buffering and flushing of messages
    // to the websocket...
    let (tx, rx) = mpsc::unbounded_channel();
    tokio::task::spawn(rx.forward(user_ws_tx).map(|result| {
        if let Err(e) = result {
            eprintln!("websocket send error: {}", e);
        }
    }));

    let head = { codebase.read().await.head.clone() };
    if let Err(_) = tx.send(Ok(Message::text(head))) {
        // it's fine
    }

    let id = pool.write().await.push(tx);

    // Every time the user sends a message, broadcast it to
    // all other users...
    while let Some(_) = user_ws_rx.next().await {
        // ignore folks
    }

    pool.write().await.drop(id);
}

enum Embed {
    Success(&'static str, Vec<u8>),
    Failure,
}

// impl futures::Future for Embed {
//     type Output = warp::reply::Response;
// }

impl warp::Reply for Embed {
    fn into_response(self) -> warp::reply::Response {
        use http::{Response, StatusCode};
        match self {
            Embed::Success(mime, body) => Response::builder()
                .header("Content-type", mime)
                .status(StatusCode::OK)
                .body(body.into())
                .unwrap(),
            Embed::Failure => {
                let mut res = http::Response::default();
                *res.status_mut() = StatusCode::NOT_FOUND;
                res
            }
        }
    }
}

fn mime_for_ext(f: &str) -> &'static str {
    if f.ends_with(".js") {
        "text/javascript"
    } else if f.ends_with(".json") {
        "application/json"
    } else if f.ends_with(".html") {
        "text/html"
    } else if f.ends_with(".css") {
        "text/css"
    } else if f.ends_with(".wasm") {
        "application/wasm"
    } else {
        "application/octet-stream"
    }
}

async fn main() {
    let pool_ref = PoolRef::default();
    let pool_for_head_message = pool_ref.clone();

    println!("Loading up the unison codebase");

    // TODO do the rebuild here I guess
    // I think I want to be able to specify (in a "get" sort of way)
    // the term that I'm building?
    // I mean, maybe that's too complicated for now.

    let (new_head_tx, new_head_rx) = tokio::sync::mpsc::unbounded_channel();

    tokio::task::spawn(async move {
        let mut new_head_rx = new_head_rx; // move it in
        while let Some(path) = new_head_rx.next().await {
            let path: std::path::PathBuf = path;
            println!("New head! {:?}", path);
            let name = path.file_name().unwrap();
            println!("New head! {:?}", name);
            pool_for_head_message
                .write()
                .await
                .broadcast(Message::text(name.to_str().unwrap()))
        }
    });

    let root = crate::pack::default_root();

    // assigning here so it doesn't get dropped
    let _watcher = {
        use notify::{RecommendedWatcher, RecursiveMode, Watcher};
        use std::sync::mpsc::channel;
        use std::time::Duration;
        let (tx, rx) = channel();
        let mut watcher: RecommendedWatcher =
            Watcher::new(tx, Duration::from_millis(200)).expect("Can't make watcher");

        // Add a path to be watched. All files and directories at that path and
        // below will be monitored for changes.
        watcher
            .watch(
                crate::pack::head_dir(root.as_path()),
                RecursiveMode::Recursive,
            )
            .expect("Can't watch");

        thread::spawn(move || loop {
            match rx.recv() {
                Ok(event) => match event {
                    notify::DebouncedEvent::Create(path) => {
                        let _ = new_head_tx.send(path);
                    }
                    _ => (),
                },
                Err(std::sync::mpsc::RecvError) => {
                    println!("Watcher finished");
                    break;
                }
            }
        });

        watcher
    };

    // Ok what the websocket does:
    // receive a new "hash" from the backend.
    // And then it requests that hash (at `/build/:hash`)
    // And then we build-on-demand.
    let codebase_ref = Arc::new(RwLock::new(
        crate::pack::load_main_branch(root.as_path()).unwrap(),
    ));
    let codebase_for_ws = codebase_ref.clone();
    let codebase_for_warp = warp::any().map(move || codebase_ref.clone());

    // TODO cache results!

    // /build/:hash/:term
    let bin = warp::path("build")
        .and(warp::path::param::<String>())
        .and(warp::path::param::<String>())
        .and(warp::path::end())
        .and(codebase_for_warp.clone())
        .and_then(|hash, term, codebase| serve_bin(hash, term, codebase));

    // /build/:hash/:term/names
    let json = warp::path("build")
        .and(warp::path::param::<String>())
        .and(warp::path::param::<String>())
        .and(warp::path("names"))
        .and(warp::path::end())
        .and(codebase_for_warp.clone())
        .and_then(|hash, term, codebase| serve_json(hash, term, codebase));

    // /build/:hash/:term/info
    let info = warp::path("build")
        .and(warp::path::param::<String>())
        .and(warp::path::param::<String>())
        .and(warp::path("info"))
        .and(warp::path::end())
        .and(codebase_for_warp.clone())
        .and_then(|hash, term, codebase| serve_info(hash, term, codebase));

    // terms/:hash
    let root_terms = warp::path("terms")
        .and(warp::path::param::<String>())
        .and(warp::path::end())
        .and(codebase_for_warp.clone())
        .and_then(|hash, codebase| serve_terms(hash, "".to_owned(), codebase));

    // terms/:hash/:ns
    let terms = warp::path("terms")
        .and(warp::path::param::<String>())
        .and(warp::path::param::<String>())
        .and(warp::path::end())
        .and(codebase_for_warp)
        .and_then(|hash, ns, codebase| serve_terms(hash, ns, codebase));

    // So I think I'll be spawning a thread for the notify thing?

    let pool = warp::any().map(move || pool_ref.clone());
    let ws = warp::path("reload-notifier")
        .and(warp::ws())
        .and(pool)
        .and(warp::any().map(move || codebase_for_ws.clone()))
        .map(|ws: warp::ws::Ws, pool, codebase| {
            ws.on_upgrade(|websocket: warp::filters::ws::WebSocket| add(pool, websocket, codebase))
        });

    let fs = warp::fs::dir(".");

    let wasm_assets = warp::fs::dir("../client/dist");
    let other_assets = warp::fs::dir("static");

    // let wasm_assets = as_filter::<WasmPkg>();
    // let other_assets = as_filter::<Asset>().or(index_filter::<Asset>());

    println!("Serving at http://127.0.0.1:3030");
    warp::serve(
        ws.or(fs)
            .or(wasm_assets)
            .or(other_assets)
            .or(bin)
            .or(json)
            .or(terms)
            .or(root_terms)
            .or(info),
    )
    .run(([127, 0, 0, 1], 3030))
    .await
}

// So, I should probably move to use these, but I want to
// do a prod build, while also

// #[derive(RustEmbed)]
// #[folder = "static"]
// struct Asset;

// #[derive(RustEmbed)]
// #[folder = "../client/dist"]
// struct WasmPkg;

#[derive(Default)]
struct TypeHashCollector(std::collections::HashSet<String>);
impl crate::visitor::Visitor for TypeHashCollector {
    fn visit_type(&mut self, typ: &mut shared::types::Type) -> bool {
        use shared::types::Type::*;
        use shared::types::*;
        match typ {
            Ref(Reference::DerivedId(Id(hash, _, _))) => {
                self.0.insert(hash.to_string());
            }
            _ => (),
        };
        true
    }
}

async fn serve_terms(
    hash: String,
    ns: String,
    codebase: Arc<RwLock<crate::branch::Codebase>>,
) -> Result<impl warp::Reply, Infallible> {
    let mut codebase = codebase.write().await;
    codebase.set_head(hash).unwrap();

    let ns = if ns == "" || ns == "." {
        codebase.head.clone()
    } else {
        codebase
            .find_ns(ns.split(".").collect::<Vec<&str>>().as_slice())
            .unwrap()
            .0
    };

    let flat_names: crate::printer::FlatNames = codebase.get_names().into();

    let (terms, children, types, constrs) = codebase.terms_and_children(&ns).unwrap();
    let mut env = crate::env::Env::init(codebase.root().as_path());
    let mut typed_terms = vec![];
    // let mut type_hashes = TypeHashCollector::default();
    for (name, hash) in terms {
        let (_source, typ) = env.load(&hash).unwrap();
        // use crate::visitor::Accept;
        // typ.accept(&mut type_hashes);

        let (args, effects, res) = typ.args_and_effects();

        let all_primitive = args.iter().all(|m| m.is_primitive()) && res.is_primitive();

        use crate::printer::ToPretty;
        typed_terms.push((
            name,
            typ.to_pretty(100, &flat_names),
            if all_primitive {
                Some((
                    args.into_iter()
                        .map(|m| shared::convert::to_json_type(m))
                        .collect::<Vec<serde_json::Value>>(),
                    effects,
                ))
            } else {
                None
            },
            hash,
        ));
    }

    Ok(serde_json::to_string_pretty(&(children, typed_terms, types, constrs)).unwrap())
}

async fn serve_info(
    hash: String,
    term: String,
    codebase: Arc<RwLock<crate::branch::Codebase>>,
) -> Result<impl warp::Reply, Infallible> {
    let mut codebase = codebase.write().await;
    codebase.set_head(hash).unwrap();
    let hash = crate::pack::find_term(&mut codebase, &term);
    let runtime_env =
        crate::pack::term_to_env(codebase.root().as_path(), &hash.to_string()).unwrap();
    let (_ir, typ) = runtime_env.terms.get(&hash).unwrap();
    Ok(serde_json::to_string_pretty(&typ.args_and_effects()).unwrap())
}

async fn serve_json(
    hash: String,
    terms: String,
    codebase: Arc<RwLock<crate::branch::Codebase>>,
) -> Result<impl warp::Reply, Infallible> {
    let mut codebase = codebase.write().await;
    codebase.set_head(hash).unwrap();
    let hashes: Vec<shared::types::Hash> = terms
        .split(",")
        .map(|term| crate::pack::find_term(&mut codebase, &term))
        .collect();
    let runtime_env = crate::pack::terms_to_env(codebase.root().as_path(), hashes).unwrap();
    Ok(serde_json::to_string_pretty(
        &crate::pack::env_names(&codebase.get_names(), &runtime_env).serialize(),
    )
    .unwrap())
}

use std::convert::Infallible;
async fn serve_bin(
    hash: String,
    terms: String,
    codebase: Arc<RwLock<crate::branch::Codebase>>,
) -> Result<impl warp::Reply, Infallible> {
    let mut codebase = codebase.write().await;
    codebase.set_head(hash).unwrap();
    let hashes: Vec<shared::types::Hash> = terms
        .split(",")
        .map(|term| crate::pack::find_term(&mut codebase, &term))
        .collect();
    let runtime_env = crate::pack::terms_to_env(codebase.root().as_path(), hashes).unwrap();

    Ok(shared::pack(&runtime_env))
}

pub fn serve() -> std::io::Result<()> {
    let mut rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(main());
    Ok(())
}
