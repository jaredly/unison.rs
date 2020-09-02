use super::types::*;
use super::types::{RuntimeEnv, IR};
use log::info;
use std::sync::Arc;

use super::frame::Source;
use super::ir_exec::Ret;
use super::stack::Stack;
use super::trace::{Trace, Traces};

static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

pub struct State<'a> {
    pub env: &'a RuntimeEnv,
    pub cmds: &'a Vec<IR>,
    pub stack: Stack,
    pub idx: usize,
}

pub fn show_env(env: &RuntimeEnv) {
    info!("[- ENV -]");
    for (k, (v, _)) in env.terms.iter() {
        info!("] Value {:?}", k);
        for (n, i) in v.iter().enumerate() {
            info!("({}) {:?}", n, i);
        }
        info!("\n");
    }
    for (i, v) in env.anon_fns.iter().enumerate() {
        info!("] Fn({}) : {:?}", i, v.0);
        for (n, i) in v.1.iter().enumerate() {
            info!("({}) {:?}", n, i)
        }
        info!("\n");
    }
}

pub fn extract_args(typ: &ABT<Type>) -> (Vec<Type>, Vec<Hash>, ABT<Type>) {
    use Type::*;
    match typ {
        ABT::Tm(typ) => match typ {
            Arrow(one, two) => {
                let (mut a, b, c) = extract_args(two);
                let one = match &**one {
                    ABT::Tm(t) => t.clone(),
                    _ => unreachable!("Not a tm {:?}", one),
                };
                a.insert(0, one.clone());
                (a, b, c)
            }
            Ann(t, _) => extract_args(t),
            t => (vec![], vec![], ABT::Tm(t.clone())),
        },
        _ => unreachable!("Um not a Tm {:?}", typ),
    }
}

pub fn eval(env: &RuntimeEnv, hash: &str, trace: &mut Traces) -> Arc<Value> {
    let mut state = State::new_value(&env, Hash::from_string(hash));
    state.run_to_end(trace)
}

impl RuntimeEnv {
    pub fn add_eval(&mut self, hash: &str, args: Vec<Value>) -> Result<Hash, String> {
        let typ = self.terms.get(&Hash::from_string(hash)).unwrap().1.clone();
        let mut cmds = vec![IR::Pop, IR::Value(Value::Ref(Reference::from_hash(hash)))];
        let (arg_typs, effects, typ) = extract_args(&typ);
        for (i, arg) in args.into_iter().enumerate() {
            if typ_check(&arg, arg_typs[i]) {
                cmds.push(IR::Value(arg));
                cmds.push(IR::Call);
            } else {
                return Err("NOPE".to_owned());
            };
        }
        let hash = Hash::from_string("<eval>");
        self.terms.insert(hash.clone(), (cmds, typ));
        Ok(hash)
    }
}

impl<'a> State<'a> {
    pub fn new_value(env: &'a RuntimeEnv, hash: Hash) -> Self {
        let source = Source::Value(hash);
        State {
            cmds: env.cmds(&source),
            stack: Stack::new(source),
            idx: 0,
            env: &env,
        }
    }

    fn run(&mut self, trace: &mut Traces, option_ref: &Reference) {
        let mut n = 0;
        while self.idx < self.cmds.len() {
            #[cfg(not(target_arch = "wasm32"))]
            if n % 100 == 0 {
                if trace.start.elapsed().as_secs() > 90 {
                    println!("Ran out of time after {} ticks", n);
                    return;
                    // let message = Arc::new(Value::Text(format!("Ran out of time after {} ticks", n)));
                    // return message;
                }
            }
            #[cfg(not(target_arch = "wasm32"))]
            let cstart = std::time::Instant::now();
            n += 1;
            let cidx = self.idx;

            let ret = self.cmds[self.idx].eval(&option_ref, &mut self.stack, &mut self.idx);

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

            self.handle_ret(ret, trace);
            self.handle_tail(trace);
        }
    }

    pub fn run_to_end(&mut self, trace: &mut Traces) -> Arc<Value> {
        let option_ref = Reference::from_hash(OPTION_HASH);

        self.run(trace, &option_ref);

        info!("Final stack: {:?}", self.stack);
        self.stack.pop().unwrap()
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

    fn handle_ret(&mut self, ret: Ret, trace: &mut Traces) {
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
            Ret::Continue(kidx, mut frames, arg) => {
                info!("** CONTINUE ** ({}) {} with {:?}", kidx, frames.len(), arg,);
                let last = frames.len() - 1;
                frames[last].return_index = self.idx;
                frames.extend(
                    self.stack
                        .frames
                        .drain(..)
                        .collect::<Vec<crate::frame::Frame>>(),
                );
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
            Ret::ReRequest(kind, number, args, final_index, frames, current_frame_idx) => {
                let (nidx, frame_index) =
                    match self.stack.back_again_to_handler(&frames, current_frame_idx) {
                        None => unreachable!("Unhandled ReRequest: {:?} / {}", kind, number),
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
            Ret::Request(kind, number, args) => {
                info!(
                    "Got a request! {:?}/{} - at {} ; self.idx {}",
                    kind, number, self.stack.frames[0], self.idx
                );
                let final_index = self.idx;
                let (nidx, saved_frames, frame_idx) = match self.stack.back_to_handler() {
                    None => unreachable!("Unhandled Request: {:?} / {}", kind, number),
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
        }
    }
}

impl RuntimeEnv {
    fn cmds(&self, source: &Source) -> &Vec<IR> {
        match source {
            Source::Value(hash) => &self.terms.get(hash).unwrap().0,
            Source::Fn(fnid, _) => &self.anon_fns[*fnid].1,
        }
    }
}
