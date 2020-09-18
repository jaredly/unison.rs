use futures::{FutureExt, StreamExt};
use tokio::sync::{mpsc, watch, RwLock};
// use std::future::Future;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use warp::filters::ws::{Message, WebSocket};
use warp::Filter;

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

// type Users = Arc<RwLock<HashMap<usize, mpsc::UnboundedSender<Result<Message, warp::Error>>>>>;
type PoolRef = Arc<RwLock<Pool>>;

async fn add(pool: PoolRef, ws: WebSocket) {
    // Split the socket into a sender and receive of messages.
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

async fn main() {
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

    warp::serve(ws.or(fs)).run(([127, 0, 0, 1], 3030)).await
}
