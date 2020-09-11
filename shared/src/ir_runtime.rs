use super::types::*;
use super::types::{RuntimeEnv, IR};
use log::info;
use std::sync::Arc;

use super::chrome_trace::{Trace, Traces};
use super::frame::{Frame, Source};
use super::ir_exec::Ret;
use super::stack::Stack;

pub static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

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

pub trait ConvertibleArg<T: Sized> {
    fn as_f64(&self) -> Option<f64>;
    fn as_string(&self) -> Option<String>;
    fn as_list(&self) -> Option<Vec<T>>;
    fn is_empty(&self) -> bool;
}

fn convert_arg<'a, T>(
    arg: T,
    typ: &'a ABT<Type>,
    mut args: Vec<&'a ABT<Type>>,
) -> Result<Value, String>
where
    T: std::fmt::Debug + Sized + ConvertibleArg<T>,
{
    use Type::*;
    use ABT::*;
    match typ {
        Var(_, _) => Ok(Value::Int(10000)),
        Tm(inner) => match inner {
            Arrow(_, _) => Err("Functions aren't yet supported".to_owned()),
            Ann(inner, _) => convert_arg(arg, inner, args),
            App(inner, targ) => {
                args.insert(0, targ);
                convert_arg(arg, inner, args)
            }
            Effect(_, _) => Err("Effect types not yet supported".to_owned()),
            Effects(_) => Err("Effects not supported".to_owned()),
            Forall(inner) => convert_arg(arg, inner, args),
            IntroOuter(inner) => convert_arg(arg, inner, args),
            Ref(Reference::Builtin(name)) => match name.as_str() {
                "Nat" => match arg.as_f64() {
                    None => Err(format!("Expected an unsigned int, got {:?}", arg)),
                    Some(n) if n < 0.0 => {
                        Err(format!("Expected an unsigned int, got a negative {}", n))
                    }
                    Some(n) if n.fract() > 1.0e-10 => {
                        Err(format!("Expected an unsigned int, got a float {}", n))
                    }
                    Some(n) => Ok(Value::Nat(n as u64)),
                },
                "Int" => match arg.as_f64() {
                    None => Err(format!("Expected an int, got {:?}", arg)),
                    Some(n) if n.fract() > 1.0e-10 => {
                        Err(format!("Expected an int, got a float {}", n))
                    }
                    Some(n) => Ok(Value::Int(n as i64)),
                },
                "Float" => match arg.as_f64() {
                    None => Err(format!("Expected a float, got {:?}", arg)),
                    Some(n) => Ok(Value::Float(n)),
                },
                "Text" => match arg.as_string() {
                    None => Err(format!("Expected a string, got {:?}", arg)),
                    Some(n) => Ok(Value::Text(n)),
                },
                _ => Err(format!("Unsupported builtin {}", name)),
            },
            Ref(Reference::DerivedId(Id(hash, _, _))) => {
                let hash_raw = hash.to_string();
                if hash_raw == OPTION_HASH {
                    match args.as_slice() {
                        [targ] => {
                            if arg.is_empty() {
                                Ok(Value::PartialConstructor(
                                    Reference::DerivedId(Id(hash.clone(), 0, 0)),
                                    0,
                                    im::Vector::new(),
                                ))
                            } else {
                                convert_arg(arg, targ, vec![])
                            }
                        }
                        _ => Err(format!("Option type can only have one argument")),
                    }
                } else {
                    Err(format!("Custom types not yet supported: {:?}", hash))
                }
            }
        },
        typ => Err(format!("Unexpected ABT {:?}", typ)),
    }
}

pub fn convert_args<T>(args: Vec<T>, typs: &Vec<ABT<Type>>) -> Result<Vec<Value>, String>
where
    T: std::fmt::Debug + Sized + ConvertibleArg<T>,
{
    if args.len() > typs.len() {
        return Err(format!(
            "Too many arguments provided: {} vs {}",
            args.len(),
            typs.len()
        ));
    }
    let mut res = vec![];
    for (i, arg) in args.into_iter().enumerate() {
        res.push(
            convert_arg(arg, &typs[i], vec![])
                .map_err(|v| format!("Unable to convert argument {}: {}", i, v))?,
        );
    }
    Ok(res)
}

pub fn extract_args(typ: &ABT<Type>) -> (Vec<ABT<Type>>, Vec<ABT<Type>>, ABT<Type>) {
    use Type::*;
    match typ {
        ABT::Abs(_, _, inner) => extract_args(inner),
        ABT::Tm(typ) => match typ {
            Forall(inner) => extract_args(inner),
            Effect(effects, inner) => {
                let (a, mut b, c) = extract_args(inner);
                let effects = match &**effects {
                    ABT::Tm(t) => match t {
                        Effects(effects) => {
                            // TODO go through and find the DerivedId refs, and just add those
                            // also dedup.
                            effects.clone()
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                b.extend(effects); // TODO do I care about ordering here?
                                   // for e in effects {
                                   //     b.push(e)
                                   // }
                (a, b, c)
            }
            Arrow(one, two) => {
                let (mut a, b, c) = extract_args(two);
                println!("ARROW {:?} => {:?}", one, two);
                // let one = match &**one {
                //     ABT::Tm(t) => t.clone(),
                //     _ => unreachable!("Not a tm {:?}", one),
                // };
                a.insert(0, (**one).clone());
                (a, b, c)
            }
            Ann(t, _) => extract_args(t),
            t => (vec![], vec![], ABT::Tm(t.clone())),
        },
        _ => unreachable!("Um not a Tm {:?}", typ),
    }
}

pub fn eval<T: FFI>(
    env: &RuntimeEnv,
    ffi: &mut T,
    hash: &str,
    trace: &mut Traces,
    do_trace: bool,
) -> Option<Arc<Value>> {
    let mut state = State::new_value(&env, Hash::from_string(hash), do_trace);
    state.run_to_end(ffi, trace)
}

// stack.back_to_handler -> `Handle(nidx, frames, etc.) | `TopLevel --- if we've bottomed out,
// how do we deal

pub trait FFI {
    // this kindof bottoms out?
    // Or I guess ...
    // hmm is there a way to do "if you can handle this inline, that would be cool"?
    // maybe like a handle_request_sync(kind, number, args), and if that gets a nope, we
    // go to handle_request_async ... that then needs to jump out of the main flow of
    // computation, waiting for a resume. Yeah that makes sense.

    // If this returns `None`, that means that the request couldn't be handled synchronously,
    // e.g. we need to just bail straight out.
    fn handle_request_sync(
        &mut self,
        kind: &Reference,
        number: usize,
        args: &Vec<Arc<Value>>,
    ) -> Option<Value>;

    // This is used at the top level, once we've bailed.
    fn handle_request(&mut self, request: FullRequest);

    fn handles(&self, kind: &Reference) -> bool;
}

impl dyn FFI {
    pub fn ensure_handles(&self, kinds: &[&Reference]) -> bool {
        for kind in kinds {
            if !self.handles(kind) {
                return false;
            }
        }
        return true;
    }
}

#[derive(Debug)]
pub struct FullRequest(
    pub Reference,
    pub usize,
    pub Vec<Arc<Value>>,
    pub Vec<Frame>,
    pub usize,
);

impl RuntimeEnv {
    pub fn add_eval(&mut self, hash: &str, args: Vec<Value>) -> Result<Hash, String> {
        let typ = self.terms.get(&Hash::from_string(hash)).unwrap().1.clone();
        let mut cmds = vec![IR::Value(Value::Ref(Reference::from_hash(hash)))];
        let (_arg_typs, _effects, typ) = extract_args(&typ);
        for (_, arg) in args.into_iter().enumerate() {
            // if typ_check(&arg, arg_typs[i]) {
            cmds.push(IR::Value(arg));
            cmds.push(IR::Call);
            // } else {
            //     return Err("NOPE".to_owned());
            // };
        }
        let hash = Hash::from_string("<eval>");
        self.terms.insert(hash.clone(), (cmds, typ));
        Ok(hash)
    }
}

impl<'a> State<'a> {
    pub fn new_value(env: &'a RuntimeEnv, hash: Hash, trace: bool) -> Self {
        let source = Source::Value(hash);
        State {
            cmds: env.cmds(&source),
            stack: Stack::new(source, trace),
            idx: 0,
            env: &env,
        }
    }

    pub fn full_resume(
        env: &'a RuntimeEnv,
        frames: Vec<Frame>,
        kidx: usize,
        arg: Arc<Value>,
    ) -> Self {
        let mut stack = Stack::from_frames(frames);
        stack.push(arg);
        State {
            env,
            cmds: env.cmds(&stack.frames[0].source),
            stack,
            idx: kidx,
        }
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
    ) -> Result<(), FullRequest> {
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
    pub fn run_to_end<T: FFI>(&mut self, ffi: &mut T, trace: &mut Traces) -> Option<Arc<Value>> {
        let option_ref = Reference::from_hash(OPTION_HASH);

        match self.run(ffi, trace, &option_ref) {
            Ok(()) => (),
            Err(request) => {
                ffi.handle_request(request);
                return None;
            }
        }

        info!("Final stack: {:?}", self.stack);
        Some(self.stack.pop().unwrap())
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

    fn handle_ret<T>(
        &mut self,
        ffi: &mut T,
        ret: Ret,
        trace: &mut Traces,
    ) -> Result<(), FullRequest>
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
            Ret::ReRequest(kind, number, args, final_index, frames, current_frame_idx) => {
                let (nidx, frame_index) = match self
                    .stack
                    .back_again_to_handler(&frames, current_frame_idx)
                {
                    None => match ffi.handle_request_sync(&kind, number, &args) {
                        None => return Err(FullRequest(kind, number, args, frames, final_index)),
                        Some(value) => {
                            self.resume(frames, final_index, Arc::new(value));
                            return Ok(());
                        }
                    },
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
                    None => match ffi.handle_request_sync(&kind, number, &args) {
                        None => {
                            return Err(FullRequest(
                                kind,
                                number,
                                args,
                                self.stack.frames.drain(..).collect(),
                                final_index,
                            ))
                        }
                        Some(value) => {
                            self.stack.push(Arc::new(value));
                            // resume(frames, final_index, value);
                            return Ok(());
                        }
                    },
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

impl RuntimeEnv {
    fn cmds(&self, source: &Source) -> &Vec<IR> {
        match source {
            Source::Value(hash) => &self.terms.get(hash).unwrap().0,
            Source::Fn(fnid, _) => &self.anon_fns[*fnid].1,
        }
    }
}
