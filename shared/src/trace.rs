use crate::frame::Source;
use crate::ir_exec::Ret;
use crate::types::{Value, IR};
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Trace {
    #[cfg(not(target_arch = "wasm32"))]
    start: std::time::Duration,
    #[cfg(not(target_arch = "wasm32"))]
    end: std::time::Duration,
    // tid, and was it a clone
    source: Option<(usize, bool)>,
    frame: Source,
    events: Vec<Event>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Event {
    Push(Value),
    Pop(Value),
    PopToMark(usize),
    PopUp,
    NewFrame(usize),
    IR(usize, IR),
    Ret(Ret),
}

#[derive(Debug)]
pub struct Traces(
    pub Vec<Trace>,
    #[cfg(not(target_arch = "wasm32"))] std::time::Instant,
);

impl Traces {
    pub fn new() -> Self {
        Traces(
            vec![],
            #[cfg(not(target_arch = "wasm32"))]
            std::time::Instant::now(),
        )
    }

    pub fn add(&mut self, source: Option<(usize, bool)>, frame: Source) -> usize {
        let n = self.0.len();
        self.0.push(Trace {
            #[cfg(not(target_arch = "wasm32"))]
            start: std::time::Instant::now() - self.1,
            #[cfg(not(target_arch = "wasm32"))]
            end: std::time::Instant::now() - self.1,
            source,
            frame,
            events: vec![],
        });
        n
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn finish(&mut self, tid: usize) {
        self.0[tid].end = std::time::Instant::now() - self.1;
    }
    #[cfg(target_arch = "wasm32")]
    pub fn finish(&mut self, tid: usize) {}

    pub fn evt(&mut self, tid: usize, evt: Event) {
        self.0[tid].events.push(evt)
    }
}
