use super::frame::{Frame, Source};
use super::types::*;
use log::info;
use std::rc::Rc;

#[derive(Debug)]
pub struct Stack {
    pub frames: Vec<Frame>,
}

impl Stack {
    pub fn new(source: Source) -> Self {
        info!("{} | Initial frame {:?}", 0, source);
        Stack {
            frames: vec![Frame::new(source, 0)],
        }
    }

    pub fn get_vbl(&mut self, sym: &Symbol, usage: usize) -> Rc<Value> {
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
        self.frames.insert(0, Frame::new(source, return_index))
    }

    pub fn clone_frame(&mut self, return_index: usize) {
        info!("{} | ----> Clone frame", self.frames.len());
        self.frames.insert(0, self.frames[0].clone());
        self.frames[0].return_index = return_index;
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    pub fn back_again_to_handler(
        &mut self,
        frames: &Vec<Frame>,
        current_idx: usize,
    ) -> Option<(usize, usize)> {
        // let mut frames = vec![];
        // self.frames.remove(0); // ignore the current one, it was a clone anyway
        let mut new_idx = current_idx + 1;
        while frames[new_idx].handler == None {
            new_idx += 1;
        }
        self.frames = frames[new_idx..].to_vec();
        // while self.frames[0].handler == None {
        //     frames.push(self.frames.remove(0));
        //     if self.frames.len() == 0 {
        //         return None;
        //     }
        // }
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        Some((idx, new_idx))
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    pub fn back_to_handler(&mut self) -> Option<(usize, Vec<Frame>, usize)> {
        let mut frames = vec![];
        while self.frames[0].handler == None {
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
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        Some((idx, frames, current_idx))
    }

    pub fn pop_frame(&mut self) -> (usize, Rc<Value>) {
        let idx = self.frames[0].return_index;
        let value = self.pop().unwrap();
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
    pub fn push(&mut self, t: Rc<Value>) {
        // info!("{} | Stack push: {:?}", self.frames.len(), t);
        self.frames[0].stack.push(t);
    }
    pub fn pop(&mut self) -> Option<Rc<Value>> {
        let t = self.frames[0].stack.pop();
        // info!("{} | Stack pop: {:?}", self.frames.len(), t);
        t
    }
    // TODO maybe return a & ref to the Rc?
    pub fn peek(&mut self) -> Option<Rc<Value>> {
        let l = self.frames[0].stack.len();
        if l > 0 {
            // info!("Stack peek: {:?}", self.frames[0].stack[l - 1]);
            Some(self.frames[0].stack[l - 1].clone())
        } else {
            None
        }
    }
    pub fn pop_to_mark(&mut self) {
        let mark = self.frames[0].marks.pop().unwrap();
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
        let ln = self.frames[0].stack.len();
        self.frames[0].stack.remove(ln - 2);
    }
}