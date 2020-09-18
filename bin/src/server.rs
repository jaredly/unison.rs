use futures::{FutureExt, StreamExt};
use tokio::sync::{mpsc, watch, RwLock};
// use std::future::Future;
use rust_embed::RustEmbed;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use warp::filters::ws::{Message, WebSocket};
use warp::Filter;

#[derive(RustEmbed)]
#[folder = "static"]
struct Asset;

#[derive(RustEmbed)]
#[folder = "../wasm/pkg"]
struct WasmPkg;

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

async fn add(pool: PoolRef, ws: WebSocket) {
    let (user_ws_tx, mut user_ws_rx) = ws.split();

    // Use an unbounded channel to handle buffering and flushing of messages
    // to the websocket...
    let (tx, rx) = mpsc::unbounded_channel();
    tokio::task::spawn(rx.forward(user_ws_tx).map(|result| {
        if let Err(e) = result {
            eprintln!("websocket send error: {}", e);
        }
    }));
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
    } else {
        "application/octet-stream"
    }
}

async fn main(path: &str) {
    let pool = PoolRef::default();
    let pool = warp::any().map(move || pool.clone());

    let ws =
        warp::path("reload-notifier")
            .and(warp::ws())
            .and(pool)
            .map(|ws: warp::ws::Ws, pool| {
                ws.on_upgrade(|websocket: warp::filters::ws::WebSocket| add(pool, websocket))
            });

    let fs = warp::fs::dir(".");
    let wasm_assets =
        warp::get().and(warp::path::tail()).map(
            |tail: warp::filters::path::Tail| match WasmPkg::get(tail.as_str()) {
                None => Embed::Failure,
                Some(value) => Embed::Success(mime_for_ext(tail.as_str()), value.into_owned()),
            },
        );

    let other_assets = warp::get().and(warp::path::tail()).map(
        |tail: warp::filters::path::Tail| match Asset::get(tail.as_str()) {
            None => Embed::Failure,
            Some(value) => Embed::Success(mime_for_ext(tail.as_str()), value.into_owned()),
        },
    );

    let res = ws.or(fs).or(wasm_assets).or(other_assets);

    println!("Ok folks");
    warp::serve(res).run(([127, 0, 0, 1], 3030)).await
}

pub fn serve(path: &str) -> std::io::Result<()> {
    println!("0");
    let mut rt = tokio::runtime::Runtime::new().unwrap();
    println!("A");
    rt.block_on(main(path));
    println!("B");
    Ok(())
}
