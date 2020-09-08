use super::frame::{Frame, Source};
use super::types::*;
use crate::trace::{Event, Traces};
use log::info;
use std::sync::Arc;

#[derive(Debug)]
pub struct Stack {
    pub frames: Vec<Frame>,
    pub traces: Traces,
    pub trace: bool,
}

impl Stack {
    pub fn new(source: Source, trace: bool) -> Self {
        info!("{} | Initial frame {:?}", 0, source);
        let mut traces = Traces::new();
        let tid = traces.add(None, source.clone());
        Stack {
            traces,
            frames: vec![Frame::new(source, 0, tid)],
            trace,
        }
    }

    pub fn get_vbl(&mut self, sym: &Symbol, usage: usize) -> Arc<Value> {
        // if this is the final usage, then pluck it out.
        let idx = match self.frames[0]
            .bindings
            .iter()
            .position(|(a, _, _)| a == sym)
        {
            None => unreachable!("Variable {:?} not found!", sym),
            Some(idx) => idx,
        };
        // TODO take usage into account
        if self.frames[0].bindings[idx].1 == usage {
            let (_, _, v) = self.frames[0].bindings.remove(idx);
            v
        } else {
            self.frames[0].bindings[idx].2.clone()
        }
    }

    pub fn new_frame(&mut self, return_index: usize, source: Source) {
        info!("{} | ----> New frame {:?}", self.frames.len(), source);
        let source_id = self.frames[0].trace_id;
        let tid = self.traces.add(Some((source_id, false)), source.clone());
        self.traces.evt(source_id, Event::NewFrame(tid));
        self.frames.insert(0, Frame::new(source, return_index, tid))
    }

    pub fn clone_frame(&mut self, return_index: usize) {
        info!("{} | ----> Clone frame", self.frames.len());
        let old_tid = self.frames[0].trace_id;
        self.frames.insert(0, self.frames[0].clone());
        self.frames[0].return_index = return_index;
        let tid = self.traces.add(
            Some((self.frames[0].trace_id, true)),
            self.frames[0].source.clone(),
        );
        self.traces.evt(old_tid, Event::CloneFrame(tid));
        self.frames[0].trace_id = tid
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    pub fn back_again_to_handler(
        &mut self,
        frames: &Vec<Frame>,
        current_idx: usize,
    ) -> Option<(usize, usize)> {
        // let mut frames = vec![];
        // self.frames.remove(0); // ignore the current one, it was a clone anyway
        let old_tid = self.frames[0].trace_id;
        let mut new_idx = current_idx + 1;
        while new_idx < frames.len() && frames[new_idx].handler == None {
            new_idx += 1;
        }
        if new_idx >= frames.len() {
            return None;
        }
        self.frames = frames[new_idx..].to_vec();
        // while self.frames[0].handler == None {
        //     frames.push(self.frames.remove(0));
        //     if self.frames.len() == 0 {
        //         return None;
        //     }
        // }
        let idx = self.frames[0].handler.expect("Not a handler (back again)");
        self.frames[0].handler = None;
        self.traces.evt(self.frames[0].trace_id, Event::HandleAgain);
        self.traces
            .evt(old_tid, Event::JumpBack(self.frames[0].trace_id));
        Some((idx, new_idx))
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    pub fn back_to_handler(&mut self) -> Option<(usize, Vec<Frame>, usize)> {
        let old_tid = self.frames[0].trace_id;
        let mut frames = vec![];
        while self.frames[0].handler == None {
            self.traces.evt(self.frames[0].trace_id, Event::Pause);
            frames.push(self.frames.remove(0));
            if self.frames.len() == 0 {
                return None;
            }
        }
        let current_idx = frames.len() - 1;
        // We need the full stack, in case this gets rethrown.
        // PERF: we could check up the stack and only do this if
        // there's another handler somewhere.
        frames.extend(self.frames.clone());
        let idx = self.frames[0].handler.expect("No handler");
        self.frames[0].handler = None;
        self.traces.evt(self.frames[0].trace_id, Event::Handle);
        self.traces
            .evt(old_tid, Event::JumpBack(self.frames[0].trace_id));
        Some((idx, frames, current_idx))
    }

    pub fn pop_frame(&mut self) -> (usize, Arc<Value>) {
        let idx = self.frames[0].return_index;
        let value = self.pop().expect("No return value to pop");
        self.traces.finish(self.frames[0].trace_id);
        self.frames.remove(0);
        info!(
            "{} | <---- Return to idx {} with value {:?} - {:?}",
            self.frames.len(),
            idx,
            value,
            self.frames[0].source
        );
        (idx, value)
    }
    // TODO : fn replace_frame
    pub fn push(&mut self, t: Arc<Value>) {
        // info!("{} | Stack push: {:?}", self.frames.len(), t);
        if self.trace {
            self.traces
                .evt(self.frames[0].trace_id, Event::Push((*t).clone()));
        }
        self.frames[0].stack.push(t);
    }
    pub fn pop(&mut self) -> Option<Arc<Value>> {
        let t = self.frames[0].stack.pop();
        if self.trace {
            let t_cloned: Value = (&t).as_ref().unwrap().as_ref().clone();
            self.traces
                .evt(self.frames[0].trace_id, Event::Pop(t_cloned));
        };
        // info!("{} | Stack pop: {:?}", self.frames.len(), t);
        t
    }
    // TODO maybe return a & ref to the Rc?
    pub fn peek(&mut self) -> Option<Arc<Value>> {
        let l = self.frames[0].stack.len();
        if l > 0 {
            // info!("Stack peek: {:?}", self.frames[0].stack[l - 1]);
            Some(self.frames[0].stack[l - 1].clone())
        } else {
            None
        }
    }

    pub fn pop_to_mark(&mut self) {
        let mark = self.frames[0].marks.pop().expect("pop to mark");
        if self.trace {
            self.traces
                .evt(self.frames[0].trace_id, Event::PopToMark(mark));
        }
        while self.frames[0].stack.len() > mark {
            self.frames[0].stack.pop();
        }
    }
    pub fn mark(&mut self) {
        let ln = self.frames[0].stack.len();
        self.frames[0].marks.push(ln);
    }
    pub fn clear_mark(&mut self) {
        self.frames[0].marks.pop();
    }
    pub fn pop_up(&mut self) {
        if self.trace {
            self.traces.evt(self.frames[0].trace_id, Event::PopUp);
        }
        let ln = self.frames[0].stack.len();
        self.frames[0].stack.remove(ln - 2);
    }
}
