use super::types::*;
use super::types::{RuntimeEnv, IR};
use log::info;
use std::collections::HashMap;
use std::sync::Arc;

use crate::chrome_trace::{Trace, Traces};
use crate::ffi::FFI;
use crate::frame::{Frame, Source};
use crate::ir_exec::Ret;
use crate::stack::Stack;

#[derive(Debug)]
pub struct InvalidLambda(usize, Value);

#[derive(Debug)]
pub struct InvalidFFI(Reference, usize, Arc<Value>);

// impl From<InvalidLambda> for Error {
//     fn from(other: InvalidLambda) -> Self {
//         Error::InvalidLambda(other)
//     }
// }

impl From<InvalidFFI> for Error {
    fn from(other: InvalidFFI) -> Self {
        Error::InvalidFFI(other)
    }
}

#[derive(Debug)]
pub struct FullRequest(
    pub Reference,
    pub usize,
    pub Vec<Arc<Value>>,
    pub Vec<Frame>,
    pub usize,
    pub ABT<Type>,
);

pub enum Error {
    Request(FullRequest),
    InvalidFFI(InvalidFFI),
    // InvalidLambda(InvalidLambda),
}

pub struct State<'a> {
    pub env: &'a RuntimeEnv,
    pub cmds: &'a Vec<IR>,
    pub stack: Stack,
    pub idx: usize,
    // effects! The concrete versions of any external effects that might be raised
    pub effects: HashMap<String, ABT<Type>>,
}

pub fn build_effects_map(
    effects: std::collections::HashSet<ABT<Type>>,
) -> HashMap<String, ABT<Type>> {
    let mut res = HashMap::new();
    for effect in effects {
        if !effect.is_var() {
            res.insert(effect.as_tm().unwrap().ref_name().unwrap(), effect);
        }
    }
    res
}

impl<'a> State<'a> {
    pub fn new_value(
        env: &'a RuntimeEnv,
        hash: Hash,
        trace: bool,
        effects: HashMap<String, ABT<Type>>,
    ) -> Self {
        let source = Source::Value(hash);
        State {
            cmds: env.cmds(&source),
            stack: Stack::new(source, trace),
            idx: 0,
            env: &env,
            effects,
        }
    }

    pub fn lambda(
        env: &'a RuntimeEnv,
        fnid: usize,
        bindings: Vec<(Symbol, usize, Arc<Value>)>,
        value: Value,
        typ: &ABT<Type>,
        effects: HashMap<String, ABT<Type>>,
    ) -> Result<Self, InvalidLambda> {
        if !crate::check::validate(Default::default(), &typ, &value).is_ok() {
            return Err(InvalidLambda(fnid, value.clone()));
        }
        let mut stack = Stack::new(Source::Fn(fnid, Hash::from_string("Lambda")), false);
        stack.frames[0].bindings = bindings;
        stack.push(Arc::new(value));
        Ok(State {
            env,
            cmds: env.cmds(&stack.frames[0].source),
            stack,
            idx: 0,
            effects,
        })
    }

    pub fn full_resume(
        env: &'a RuntimeEnv,
        kind: Reference,
        constructor_index: usize,
        frames: Vec<Frame>,
        kidx: usize,
        arg: Arc<Value>,
    ) -> Result<Self, InvalidFFI> {
        let constructor_type = env.get_ability_type(&kind, constructor_index);
        let (_arg_types, effects, return_type) = crate::ir_runtime::extract_args(&constructor_type);

        if !crate::check::validate(Default::default(), &return_type, &*arg).is_ok() {
            return Err(InvalidFFI(kind, constructor_index, arg.clone()));
        }
        let mut stack = Stack::from_frames(frames);
        stack.push(arg);
        Ok(State {
            env,
            cmds: env.cmds(&stack.frames[0].source),
            stack,
            idx: kidx,
            effects: build_effects_map(effects),
        })
    }

    fn resume(&mut self, mut frames: Vec<Frame>, kidx: usize, arg: Arc<Value>) {
        let last = frames.len() - 1;
        frames[last].return_index = self.idx;
        frames.extend(self.stack.frames.drain(..).collect::<Vec<Frame>>());
        self.stack.frames = frames;
        info!("New Top Frame: {}", self.stack.frames[0]);
        info!("Handlers:");
        let ln = self.stack.frames.len();
        for (i, frame) in self.stack.frames.iter().enumerate() {
            if frame.handler != None {
                info!("{} | {}", ln - i, frame);
            }
        }
        self.idx = kidx;
        self.stack.push(arg);
        self.cmds = self.env.cmds(&self.stack.frames[0].source);
    }

    fn run<T: FFI>(
        &mut self,
        ffi: &mut T,
        trace: &mut Traces,
        option_ref: &Reference,
    ) -> Result<(), Error> {
        #[cfg(not(target_arch = "wasm32"))]
        let mut n = 0;
        while self.idx < self.cmds.len() {
            #[cfg(not(target_arch = "wasm32"))]
            if n % 100 == 0 {
                if trace.start.elapsed().as_secs() > 90 {
                    println!("Ran out of time after {} ticks", n);
                    return Ok(());
                    // let message = Arc::new(Value::Text(format!("Ran out of time after {} ticks", n)));
                    // return message;
                }
            }
            #[cfg(not(target_arch = "wasm32"))]
            let cstart = std::time::Instant::now();
            #[cfg(not(target_arch = "wasm32"))]
            {
                n += 1;
            };
            let cidx = self.idx;

            let tid = self.stack.frames[0].trace_id;
            self.stack
                .traces
                .evt(tid, crate::trace::Event::IR(cidx, self.cmds[cidx].clone()));

            let ret = self.cmds[self.idx].eval(&option_ref, &mut self.stack, &mut self.idx);

            match &ret {
                Ret::Nothing => (),
                ret => {
                    self.stack
                        .traces
                        .evt(tid, crate::trace::Event::Ret(ret.clone()));
                }
            };

            #[cfg(not(target_arch = "wasm32"))]
            {
                let ctime = cstart.elapsed();
                if ctime.as_millis() > 1 {
                    trace.traces.push(Trace {
                        cat: "Instruction".to_owned(),
                        ph: "B".to_owned(),
                        ts: trace.start.elapsed() - ctime,
                        name: (self.stack.frames[0].source.clone(), Some(cidx)),
                        file: "".to_owned(),
                        tid: 1,
                    });
                    trace.traces.push(Trace {
                        cat: "Instruction".to_owned(),
                        ph: "E".to_owned(),
                        ts: trace.start.elapsed(),
                        name: (self.stack.frames[0].source.clone(), Some(cidx)),
                        file: "".to_owned(),
                        tid: 1,
                    });
                };
            };

            self.handle_ret(ffi, ret, trace)?;
            self.handle_tail(trace);
        }
        Ok(())
    }

    // If it was able to complete synchronously, you get the final value
    // Otherwise, you get None
    pub fn run_to_end<T: FFI>(
        &mut self,
        ffi: &mut T,
        trace: &mut Traces,
    ) -> Result<Option<Arc<Value>>, InvalidFFI> {
        let option_ref = Reference::from_hash(crate::convert::OPTION_HASH);

        match self.run(ffi, trace, &option_ref) {
            Ok(()) => (),
            Err(Error::Request(request)) => {
                ffi.handle_request(request);
                return Ok(None);
            }
            Err(Error::InvalidFFI(error)) => return Err(error),
            // Err(Error::InvalidLambda(error)) => return Err(error),
        }

        info!("Final stack: {:?}", self.stack);
        Ok(Some(self.stack.pop().unwrap()))
    }

    fn handle_tail(&mut self, trace: &mut Traces) {
        while self.idx >= self.cmds.len() {
            #[cfg(not(target_arch = "wasm32"))]
            trace.push(&self.stack.frames[0], "E");
            if self.stack.frames.len() > 1 {
                let (idx1, value) = self.stack.pop_frame();
                self.idx = idx1;
                self.stack.push(value);
                self.cmds = self.env.cmds(&self.stack.frames[0].source);
            } else {
                info!("Got only one frame left, and self.idx is larger than the self.cmds len");
                break;
            }
        }
    }

    fn handle_ret<T>(&mut self, ffi: &mut T, ret: Ret, trace: &mut Traces) -> Result<(), Error>
    where
        T: FFI,
    {
        match ret {
            Ret::Nothing => (),
            Ret::Handle(mark_idx) => {
                self.idx += 1;
                info!(
                    "{} | Setting handle, mark self.idx {}",
                    self.stack.frames.len(),
                    mark_idx
                );
                if self.stack.frames[0].handler != None {
                    unreachable!("Can't set a handle on a frame that already has one...");
                }
                self.stack.frames[0].handler = Some(mark_idx);
                let ln = self.stack.frames.len();
                for (i, frame) in self.stack.frames.iter().enumerate() {
                    if frame.handler != None {
                        info!("{} | {}", ln - i, frame);
                    }
                }
                self.stack.clone_frame(mark_idx);
                self.stack.frames[0].handler = None;
            }
            Ret::Continue(kidx, frames, arg) => {
                info!("** CONTINUE ** ({}) {} with {:?}", kidx, frames.len(), arg,);
                self.resume(frames, kidx, arg);
                // let last = frames.len() - 1;
                // frames[last].return_index = self.idx;
                // frames.extend(self.stack.frames.drain(..).collect::<Vec<Frame>>());
                // self.stack.frames = frames;
                // info!("New Top Frame: {}", self.stack.frames[0]);
                // info!("Handlers:");
                // let ln = self.stack.frames.len();
                // for (i, frame) in self.stack.frames.iter().enumerate() {
                //     if frame.handler != None {
                //         info!("{} | {}", ln - i, frame);
                //     }
                // }
                // self.idx = kidx;
                // self.stack.push(arg);
                // self.cmds = self.env.cmds(&self.stack.frames[0].source);
            }
            Ret::ReRequest(kind, number, mut args, final_index, frames, current_frame_idx) => {
                let (nidx, frame_index) = match self
                    .stack
                    .back_again_to_handler(&frames, current_frame_idx)
                {
                    None => {
                        let constructor_type = self.env.get_ability_type(&kind, number);
                        let (arg_types, _effects, return_type) =
                            crate::ir_runtime::extract_args(&constructor_type);
                        // for any partial functions, annotate them with the type
                        // for any partial functions, annotate them with the type
                        for (i, arg) in args.iter_mut().enumerate() {
                            match &**arg {
                                Value::PartialFnBody(fnid, bindings) => {
                                    *arg = Arc::new(Value::PartialFnBodyWithType(
                                        *fnid,
                                        bindings.clone(),
                                        arg_types[i].clone(),
                                    ))
                                }
                                _ => (),
                            }
                        }
                        // ok folks

                        match ffi.handle_request_sync(&constructor_type, &kind, number, &args) {
                            None => {
                                return Err(Error::Request(FullRequest(
                                    kind,
                                    number,
                                    args,
                                    frames,
                                    final_index,
                                    return_type,
                                )))
                            }
                            Some(value) => {
                                // OH TODO ok folks lets just bail here if the type from javascript is wrong
                                // we have the power, folks.
                                // If you're doing an FFI, we have the right to take it to pieces.
                                // I guess that means I need to be able to return a different kind of
                                // ship-stopping error
                                if !crate::check::validate(Default::default(), &return_type, &value)
                                    .is_ok()
                                {
                                    return Err(Error::InvalidFFI(InvalidFFI(
                                        kind,
                                        number,
                                        Arc::new(value),
                                    )));
                                }
                                self.resume(frames, final_index, Arc::new(value));
                                return Ok(());
                            }
                        }
                    }
                    Some((a, b)) => (a, b),
                };
                self.idx = nidx;
                info!(
                    "Handling a bubbled request : {} - {}",
                    self.idx, self.stack.frames[0]
                );

                self.cmds = self.env.cmds(&self.stack.frames[0].source);

                self.stack.push(Arc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    frames,
                    frame_index,
                )))
            }
            Ret::Request(kind, number, mut args) => {
                info!(
                    "Got a request! {:?}/{} - at {} ; self.idx {}",
                    kind, number, self.stack.frames[0], self.idx
                );
                let final_index = self.idx;
                let (nidx, saved_frames, frame_idx) = match self.stack.back_to_handler() {
                    None => {
                        let constructor_type = self.env.get_ability_type(&kind, number);
                        let concrete_type = self
                            .effects
                            .get(&kind.hash().expect("Not a DerivedId").to_string())
                            .expect("No effect found");
                        info!("COncrete type: {:?}", concrete_type);
                        let constructor_args = concrete_type.as_tm().unwrap().app_args();
                        let (arg_types, _effects, return_type) =
                            crate::ir_runtime::extract_args(&constructor_type);
                        // for any partial functions, annotate them with the type
                        for (i, arg) in args.iter_mut().enumerate() {
                            match &**arg {
                                Value::PartialFnBody(fnid, bindings) => {
                                    *arg = Arc::new(Value::PartialFnBodyWithType(
                                        *fnid,
                                        bindings.clone(),
                                        arg_types[i].clone(),
                                    ))
                                }
                                _ => (),
                            }
                        }
                        // ok folks

                        match ffi.handle_request_sync(&return_type, &kind, number, &args) {
                            None => {
                                return Err(Error::Request(FullRequest(
                                    kind,
                                    number,
                                    args,
                                    self.stack.frames.drain(..).collect(),
                                    final_index,
                                    return_type,
                                )))
                            }
                            Some(value) => {
                                if !crate::check::validate(Default::default(), &return_type, &value)
                                    .is_ok()
                                {
                                    return Err(Error::InvalidFFI(InvalidFFI(
                                        kind,
                                        number,
                                        Arc::new(value),
                                    )));
                                }

                                self.stack.push(Arc::new(value));
                                return Ok(());
                            }
                        }
                    }
                    Some((a, b, c)) => (a, b, c),
                };
                self.idx = nidx;
                info!(
                    "Found handler at frame {} - {:?} - self.idx {}",
                    self.stack.frames.len(),
                    self.stack.frames[0].source,
                    self.idx
                );

                self.cmds = self.env.cmds(&self.stack.frames[0].source);

                self.stack.push(Arc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    saved_frames,
                    frame_idx,
                )))
            }
            Ret::FnCall(fnid, bindings, arg) => {
                self.cmds = &self.env.anon_fns[fnid].1;

                self.stack.new_frame(
                    self.idx,
                    Source::Fn(fnid, self.env.anon_fns[fnid].0.clone()),
                );
                #[cfg(not(target_arch = "wasm32"))]
                trace.push(&self.stack.frames[0], "B");
                self.stack.frames[0].bindings = bindings;
                self.stack.frames[0].stack.push(arg);
                self.idx = 0;
            }
            Ret::Value(hash) => {
                self.cmds = &self.env.terms.get(&hash).unwrap().0;
                self.stack.new_frame(self.idx, Source::Value(hash));
                #[cfg(not(target_arch = "wasm32"))]
                trace.push(&self.stack.frames[0], "B");
                self.idx = 0;
            }
            Ret::HandlePure => {
                let (idx1, value) = self.stack.pop_frame();
                self.idx = idx1;
                self.stack.push(value);
                self.cmds = self.env.cmds(&self.stack.frames[0].source);
            }
        };
        Ok(())
    }
}
