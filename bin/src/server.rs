use futures::{FutureExt, StreamExt};
use tokio::sync::{mpsc, watch, RwLock};
// use std::future::Future;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use warp::filters::ws::{Message, WebSocket};
use warp::Filter;

type Receiver = watch::Receiver<Message>;

// type Users = Arc<RwLock<HashMap<usize, mpsc::UnboundedSender<Result<Message, warp::Error>>>>>;

async fn main() {
    let (sender, receiver) = watch::channel(Message::text("hi"));
    let receiver = warp::any().map(|| receiver.clone());

    let ws = warp::path("reload-notifier")
        // The `ws()` filter will prepare the Websocket handshake.
        .and(warp::ws())
        .and(receiver)
        .map(|ws: warp::ws::Ws, receiver: Receiver| {
            let x = move |websocket: warp::filters::ws::WebSocket| {
                // Just echo all messages back...
                let (tx, rx) = websocket.split();
                // let m: futures_util::stream::Forward<futures::channel::mpsc::Receiver<()>, ()> =
                receiver.map(|m| Ok(m)).forward(tx).map(|result| {
                    if let Err(e) = result {
                        eprintln!("websocket error: {:?}", e);
                    }
                })
                // m
                // *inner.lock().unwrap() = Some(tx);
                // async { () }
                // rx.forward(tx).map(|result| {
                //     if let Err(e) = result {
                //         eprintln!("websocket error: {:?}", e);
                //     }
                // })
            };
            // And then our closure will be called when it completes...
            ws.on_upgrade(x)
        });

    let fs = warp::fs::dir(".");

    warp::serve(fs).run(([127, 0, 0, 1], 3030)).await
}
