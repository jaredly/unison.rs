use super::ir::{GlobalEnv, IR};
use super::types::*;
use im::Vector;
use log::info;
use serde_derive::{Deserialize, Serialize};
use std::rc::Rc;
// use tracing::{debug, error, info as info_, span, warn, Level};

static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

#[derive(Debug, Clone, std::cmp::PartialEq, std::cmp::PartialOrd, Serialize, Deserialize, Hash)]
pub enum Source {
    Value(Hash),
    Fn(usize, Hash),
}

#[derive(Debug, Clone)]
pub struct Frame {
    // span: tracing::span:
    source: Source,
    stack: Vec<Rc<Value>>,
    marks: Vec<usize>,
    handler: Option<usize>,
    return_index: usize,
    bindings: Vec<(Symbol, usize, Rc<Value>)>, // the number of usages to expect
}

impl std::cmp::PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        self.source == other.source
    }
}

impl std::cmp::PartialOrd for Frame {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.source < other.source {
            Some(std::cmp::Ordering::Less)
        } else if self.source > other.source {
            Some(std::cmp::Ordering::Greater)
        } else {
            None
        }
    }
}

impl Frame {
    fn new(source: Source, return_index: usize) -> Self {
        // let span = span!(Level::TRACE, format!("{:?}", source));
        Frame {
            source,
            stack: vec![],
            marks: vec![],
            handler: None,
            return_index,
            bindings: vec![],
        }
    }

    fn as_trace(&self, ph: &str, ts: std::time::Duration) -> Trace {
        Trace {
            cat: "frame".to_owned(),
            ph: ph.to_owned(), // B(eginning) E(nd) or I (info)
            ts,
            name: (self.source.clone(), None),
            tid: 1,                  // thread ID I think?
            file: "main".to_owned(), // [file] // TODO
        }
    }
}

#[derive(Debug)]
struct Stack {
    frames: Vec<Frame>,
}
impl Stack {
    fn new(source: Source) -> Self {
        info!("{})> Initial frame {:?}", 0, source);
        Stack {
            frames: vec![Frame::new(source, 0)],
        }
    }

    fn get_vbl(&mut self, sym: &Symbol, usage: usize) -> Rc<Value> {
        // if this is the final usage, then pluck it out.
        let idx = self.frames[0]
            .bindings
            .iter()
            .position(|(a, _, _)| a == sym)
            .expect("Variable not found");
        // TODO take usage into account
        if self.frames[0].bindings[idx].1 == usage {
            let (_, _, v) = self.frames[0].bindings.remove(idx);
            v
        } else {
            self.frames[0].bindings[idx].2.clone()
        }
    }

    fn new_frame(&mut self, return_index: usize, source: Source) {
        info!("{} | ----> New frame {:?}", self.frames.len(), source);
        self.frames.insert(0, Frame::new(source, return_index))
    }

    fn clone_frame(&mut self, return_index: usize) {
        info!("{} | ----> Clone frame", self.frames.len());
        self.frames.insert(0, self.frames[0].clone());
        self.frames[0].return_index = return_index;
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    fn back_again_to_handler(&mut self) -> Option<(usize, Vec<Frame>)> {
        let mut frames = vec![];
        self.frames.remove(0); // ignore the current one, it was a clone anyway
        while self.frames[0].handler == None {
            frames.push(self.frames.remove(0));
            if self.frames.len() == 0 {
                // unreachable!("Unhandled effect thrown: {:?}",)
                return None;
            }
        }
        // frames.extend(self.frames.clone());
        // frames.push(self.frames[0].clone()); // TODO change return pointer I think?
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        Some((idx, frames))
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    fn back_to_handler(&mut self) -> Option<(usize, Vec<Frame>)> {
        let mut frames = vec![];
        while self.frames[0].handler == None {
            frames.push(self.frames.remove(0));
            if self.frames.len() == 0 {
                // unreachable!("Unhandled effect thrown")
                return None;
            }
        }
        // frames.extend(self.frames.clone());
        // frames.push(self.frames[0].clone()); // TODO change return pointer I think?
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        Some((idx, frames))
    }

    fn pop_frame(&mut self) -> (usize, Rc<Value>) {
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
    fn push(&mut self, t: Rc<Value>) {
        info!("{} | Stack push: {:?}", self.frames.len(), t);
        self.frames[0].stack.push(t);
    }
    fn pop(&mut self) -> Option<Rc<Value>> {
        let t = self.frames[0].stack.pop();
        info!("{} | Stack pop: {:?}", self.frames.len(), t);
        t
    }
    // TODO maybe return a & ref to the Rc?
    fn peek(&mut self) -> Option<Rc<Value>> {
        let l = self.frames[0].stack.len();
        if l > 0 {
            info!("Stack peek: {:?}", self.frames[0].stack[l - 1]);
            Some(self.frames[0].stack[l - 1].clone())
        } else {
            None
        }
    }
    fn pop_to_mark(&mut self) {
        let mark = self.frames[0].marks.pop().unwrap();
        while self.frames[0].stack.len() > mark {
            self.frames[0].stack.pop();
        }
    }
    fn mark(&mut self) {
        let ln = self.frames[0].stack.len();
        self.frames[0].marks.push(ln);
    }
    fn clear_mark(&mut self) {
        self.frames[0].marks.pop();
    }
    fn pop_up(&mut self) {
        let ln = self.frames[0].stack.len();
        self.frames[0].stack.remove(ln - 2);
    }
}

// pub enum TraceSource {
//     Source(Source),
//     Instruction(Source, )
// }

pub struct Trace {
    pub cat: String,
    pub ph: String, // B(eginning) E(nd) or I (info)
    // pid: 1
    pub ts: std::time::Duration,
    pub name: (Source, Option<usize>),
    pub tid: usize,   // thread ID I think?
    pub file: String, // [file]
}

#[allow(while_true)]
pub fn eval(env: GlobalEnv, hash: &str, trace: &mut Vec<Trace>) -> Rc<Value> {
    let hash = Hash::from_string(hash);
    info!("[- ENV -]");
    for (k, v) in env.terms.iter() {
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

    let mut cmds: &Vec<IR> = env.terms.get(&hash).unwrap();

    let mut stack = Stack::new(Source::Value(hash.clone()));

    let option_ref = Reference::from_hash(OPTION_HASH);

    let mut idx = 0;

    let mut n = 0;

    // let mut longest = 10;
    // let mut last = std::time::Instant::now();

    let start = std::time::Instant::now();

    while idx < cmds.len() {
        // let t = last.elapsed().as_millis();
        // if t > longest {
        //     longest = t;
        //     println!("Longest {}", t);
        // }
        // last = std::time::Instant::now();
        if n % 100 == 0 {
            if start.elapsed().as_secs() > 20 {
                let n = Rc::new(Value::Text(format!("Ran out of time after {} ticks", n)));
                return n;
            }
        }

        let cstart = std::time::Instant::now();
        let cidx = idx;

        n += 1;
        // if n % 300 == 0 {
        //     println!("{}", n);
        // }
        // print!(".");
        // if n > 10_000 {
        //     println!("\n< > < > Sorry folks < > < >\n");
        //     return Value::Int(0);
        // }

        let cmd = &cmds[idx];
        info!("----- <{}>    {:?}", idx, cmd);

        let ret = cmd.eval(&option_ref, &mut stack, &mut idx);

        let ctime = cstart.elapsed();
        if ctime.as_millis() > 1 {
            trace.push(Trace {
                cat: "Instruction".to_owned(),
                ph: "B".to_owned(),
                ts: start.elapsed() - ctime,
                name: (stack.frames[0].source.clone(), Some(cidx)),
                file: "".to_owned(),
                tid: 1,
            });

            trace.push(Trace {
                cat: "Instruction".to_owned(),
                ph: "E".to_owned(),
                ts: start.elapsed(),
                name: (stack.frames[0].source.clone(), Some(cidx)),
                file: "".to_owned(),
                tid: 1,
            });
        };

        match ret {
            Ret::Nothing => (),
            Ret::Handle(mark_idx) => {
                idx += 1;
                info!("Setting handle, mark idx {}", mark_idx);
                stack.frames[0].handler = Some(mark_idx);
                stack.clone_frame(mark_idx);
                stack.frames[0].handler = None;
            }
            Ret::Continue(kidx, mut frames, arg) => {
                info!("** CONTINUE ** ({}) {} with {:?}", kidx, frames.len(), arg,);
                let last = frames.len() - 1;
                frames[last].return_index = idx; // ok I think this is the return pointer?
                stack.frames = {
                    frames.extend(stack.frames);
                    frames
                };
                // stack.frames = frames;
                for frame in &stack.frames {
                    info!(
                        "> {:?} : returning to index {} below",
                        frame.source, frame.return_index
                    );
                }
                idx = kidx;
                stack.push(arg);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                }
                // umm
                // ok, so do we need to clone the absolutely whole stack?
                // Or .. is it just "when we go down a level, we need to clone"
                // well let's just clone everything to start, because why not I guess?
            }
            Ret::ReRequest(kind, number, args, final_index, mut frames) => {
                let (nidx, saved_frames) = match stack.back_again_to_handler() {
                    None => unreachable!("Unhandled ReRequest: {:?} / {}", kind, number),
                    Some((a, b)) => (a, b),
                };
                frames.extend(saved_frames);
                idx = nidx;
                info!(
                    "Handling a bubbled request : {} - {:?}",
                    idx, stack.frames[0].source
                );

                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };

                stack.push(Rc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    frames,
                )))
            }
            Ret::Request(kind, number, args) => {
                let final_index = idx;
                // So, the continuation will always contain the top level, even if the handler is on the top level.
                // So we can't just "pop" things, unfortunately.
                // Right?
                // Or. ... maybe we should add a new frame when we pass through a handler?
                // hrm, yeah, seems like if we want to be able to keep stack variables the way they were ...
                // ok, so we add a new frame, that's a clone of the old frame, hm.
                // Which means, every frame will have at most one handler.
                let (nidx, saved_frames) = match stack.back_to_handler() {
                    None => unreachable!("Unhandled Request: {:?} / {}", kind, number),
                    Some((a, b)) => (a, b),
                };
                idx = nidx;
                info!(
                    "Jumping back to idx {} to {:?}",
                    idx, stack.frames[0].source
                );

                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };

                stack.push(Rc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    saved_frames,
                )))
            }
            Ret::FnCall(fnid, bindings, arg) => {
                cmds = &env.anon_fns[fnid].1;

                stack.new_frame(idx, Source::Fn(fnid, env.anon_fns[fnid].0.clone()));
                trace.push(stack.frames[0].as_trace("B", start.elapsed()));
                info!("^ fn call with {:?}", arg);
                stack.frames[0].bindings = bindings;
                stack.frames[0].stack.push(arg);
                // // cmds = env.anon_fns.get(&hash.to_string()).unwrap();
                // info!("[Fn Instructions - {}]", fnid);
                // for cmd in cmds {
                //     info!("{:?}", cmd);
                // }
                idx = 0;
            }
            Ret::Value(hash) => {
                // info!("Jumping to {:?}", hash);
                cmds = env.terms.get(&hash).unwrap();
                stack.new_frame(idx, Source::Value(hash));
                trace.push(stack.frames[0].as_trace("B", start.elapsed()));
                // info!("[Value Instructions]");
                // for cmd in cmds {
                //     info!("{:?}", cmd);
                // }
                idx = 0;
            }
            Ret::HandlePure => {
                let (idx1, value) = stack.pop_frame();
                idx = idx1;
                // stack.pop_frame();
                // stack.frames.remove(0);
                stack.push(value);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };
            }
        }

        // if time > 5 {
        //     println!("Took: {}", time);
        //     println!("{:?}", cmd);
        // }

        // Multi-pop
        while idx >= cmds.len() {
            trace.push(stack.frames[0].as_trace("E", start.elapsed()));
            if stack.frames.len() > 1 {
                // info!("<<-- jump down");
                let (idx1, value) = stack.pop_frame();
                idx = idx1;
                // stack.pop_frame();
                // stack.frames.remove(0);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => {
                        // info!("Going back to {}", hash);
                        stack.push(value);
                        cmds = env.terms.get(&hash).unwrap();
                    }
                    Source::Fn(fnid, _) => {
                        // info!("Going back to fn {}", fnid);
                        stack.push(value);
                        cmds = &env.anon_fns[fnid].1;
                    }
                }
            } else {
                info!("Got only one frame left, and idx is larger than the cmds len");
                break;
            }
        }
    }

    info!("Final stack: {:?}", stack);
    stack.pop().unwrap()
}

enum Ret {
    FnCall(usize, Vec<(Symbol, usize, Rc<Value>)>, Rc<Value>),
    Value(Hash),
    Nothing,
    Request(Reference, usize, Vec<Rc<Value>>),
    ReRequest(Reference, usize, Vec<Rc<Value>>, usize, Vec<Frame>),
    Handle(usize),
    HandlePure,
    Continue(usize, Vec<Frame>, Rc<Value>),
}

impl IR {
    fn eval(
        &self,
        // env: &GlobalEnv,
        option_ref: &Reference,
        stack: &mut Stack,
        idx: &mut usize,
        // marks: &HashMap<usize, usize>,
    ) -> Ret {
        match self {
            IR::Swap => {
                let one = stack.pop().unwrap();
                let two = stack.pop().unwrap();
                stack.push(one);
                stack.push(two);
                *idx += 1;
            }
            IR::Handle(mark) => return Ret::Handle(*mark),
            IR::HandlePure => {
                let v = stack.pop().unwrap();
                let v = match *v {
                    Value::RequestPure(_) => v,
                    _ => Rc::new(Value::RequestPure(v)),
                };
                stack.push(v);
                // *idx += 1;
                return Ret::HandlePure;
                // return Ret::RequestPure(v);
            }
            IR::Value(term) => {
                match term {
                    Value::Request(a, b) => {
                        *idx += 1;
                        return Ret::Request(a.clone(), *b, vec![]);
                    }
                    Value::RequestWithArgs(a, b, n, args) if *n == args.len() => {
                        *idx += 1;
                        return Ret::Request(a.clone(), *b, args.clone());
                    }
                    Value::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                        // Jump!
                        *idx += 1;
                        return Ret::Value(hash.clone());
                        // let value = env.terms.get(&hash.to_string()).clone();
                        // stack.push_frame(idx, hash.to_string());
                        // *idx = 0;
                        // cmds = value;
                        // JUMP!
                    }
                    _ => {
                        stack.push(Rc::new(term.clone()));
                        *idx += 1;
                    }
                };
            }
            // IR::Reference()
            // IR::Ref(hash) => {
            //     stack.push(Value::Ref(Reference::))
            //     // let res = env.load(&hash.to_string());
            // }
            // IR::Builtin(name) => {
            //     stack.push(Value::Ref(Reference::Builtin(name.clone())));
            //     *idx += 1;
            // }
            IR::PushSym(symbol, usage) => {
                let v = stack.get_vbl(symbol, *usage);
                stack.push(v);
                // let i = match stack.frames[0]
                //     .bindings
                //     .iter()
                //     .position(|(k, _, _)| symbol == k)
                // {
                //     None => unreachable!("Vbl not found {}", symbol.text),
                //     Some(idx) => idx,
                // };
                // stack.push(stack.frames[0].bindings[i].1.clone());
                *idx += 1;
            }
            IR::PopAndName(symbol, uses) => {
                let v = stack.pop().unwrap();
                stack.frames[0]
                    .bindings
                    .insert(0, (symbol.clone(), *uses, v));
                *idx += 1;
            }
            IR::Fn(i, free_vbls) => {
                let bound: Vec<(Symbol, usize, Rc<Value>)> = free_vbls
                    .iter()
                    .enumerate()
                    .map(|(i, (sym, max))| (sym.with_unique(i), *max, stack.get_vbl(sym, *max)))
                    .collect();
                stack.push(Rc::new(Value::PartialFnBody(*i, bound)));
                *idx += 1;
            }
            IR::Cycle(names) => {
                let mut mutuals = vec![];
                let mut items = vec![];
                for (name, uses) in names {
                    let v = stack.pop().unwrap();
                    match &*v {
                        Value::PartialFnBody(fnint, bindings) => {
                            mutuals.push((name.clone(), *uses, *fnint));
                            items.push((name, *uses, *fnint, bindings.clone()));
                        }
                        v => {
                            stack.frames[0].bindings.push((
                                name.clone(),
                                *uses,
                                Rc::new(v.clone()),
                            ));
                        }
                    }
                }
                for (name, uses, fnint, bindings) in items {
                    stack.frames[0].bindings.push((
                        name.clone(),
                        uses,
                        Rc::new(Value::CycleFnBody(fnint, bindings, mutuals.clone())),
                    ))
                }

                // stack.push(Value::CycleFnBody(
                //     *i,
                //     stack.frames[0].bindings.clone(),
                //     mutuals.clone(),
                // ));
                *idx += 1;
            }
            // IR::CycleFn(i, mutuals) => {
            //     stack.push(Value::CycleFnBody(
            //         *i,
            //         stack.frames[0].bindings.clone(),
            //         mutuals.clone(),
            //     ));
            //     *idx += 1;
            // }
            IR::Call => {
                info!("Call");
                let arg = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                match &*f {
                    Value::Continuation(kidx, frames) => {
                        *idx += 1;
                        return Ret::Continue(*kidx, frames.clone(), arg);
                    }
                    Value::RequestWithArgs(r, i, n, args) => {
                        *idx += 1;
                        let mut args = args.clone();
                        args.push(arg);
                        if args.len() == *n {
                            return Ret::Request(r.clone(), *i, args);
                        }
                        info!("- request - {:?} - {}", args, n);
                        stack.push(Rc::new(Value::RequestWithArgs(r.clone(), *i, *n, args)));
                    }
                    Value::Constructor(r, u) => {
                        stack.push(Rc::new(Value::PartialConstructor(
                            r.clone(),
                            *u,
                            Vector::from(vec![arg]),
                        )));
                        *idx += 1;
                    }
                    Value::PartialConstructor(r, u, c) => {
                        let mut c = c.clone();
                        c.push_back(arg);
                        stack.push(Rc::new(Value::PartialConstructor(r.clone(), *u, c)));
                        *idx += 1;
                    }
                    Value::CycleFnBody(fnint, bindings, mutuals) => {
                        *idx += 1;
                        let mut bindings = bindings.clone();
                        let copy = bindings.clone();
                        for (k, uses, v) in mutuals {
                            bindings.push((
                                k.clone(),
                                *uses,
                                Rc::new(Value::CycleFnBody(*v, copy.clone(), mutuals.clone())),
                            ))
                        }
                        return Ret::FnCall(*fnint, bindings, arg);
                    }
                    Value::PartialFnBody(fnint, bindings) => {
                        *idx += 1;
                        return Ret::FnCall(*fnint, bindings.clone(), arg);
                    }
                    Value::Ref(Reference::Builtin(builtin)) => {
                        let res = match (builtin.as_str(), &*arg) {
                            ("Int.increment", Value::Int(i)) => Some(Value::Int(i + 1)),
                            ("Int.negate", Value::Int(i)) => Some(Value::Int(-i)),
                            ("Int.isEven", Value::Int(i)) => Some(Value::Boolean(i % 2 == 0)),
                            ("Int.isOdd", Value::Int(i)) => Some(Value::Boolean(i % 2 == 1)),
                            ("Nat.increment", Value::Nat(i)) => Some(Value::Nat(i + 1)),
                            ("Nat.isEvent", Value::Nat(i)) => Some(Value::Boolean(i % 2 == 0)),
                            ("Nat.isOdd", Value::Nat(i)) => Some(Value::Boolean(i % 2 == 1)),
                            ("Nat.toInt", Value::Nat(i)) => Some(Value::Int(*i as i64)),
                            ("Nat.toText", Value::Nat(i)) => Some(Value::Text(i.to_string())),
                            ("Boolean.not", Value::Boolean(i)) => Some(Value::Boolean(!i)),
                            ("List.size", Value::Sequence(s)) => Some(Value::Nat(s.len() as u64)),
                            ("Text.size", Value::Text(t)) => Some(Value::Nat(t.len() as u64)),
                            ("Text.toCharList", Value::Text(t)) => Some(Value::Sequence(
                                t.chars().map(|c| Rc::new(Value::Char(c))).collect(),
                            )),
                            ("Text.fromCharList", Value::Sequence(l)) => Some(Value::Text({
                                l.iter()
                                    .map(|c| match **c {
                                        Value::Char(c) => c.clone(),
                                        _ => unreachable!("Not a char"),
                                    })
                                    .collect()
                            })),
                            ("Bytes.size", Value::Bytes(t)) => Some(Value::Nat(t.len() as u64)),
                            ("Bytes.toList", Value::Bytes(t)) => Some(Value::Sequence(
                                t.iter().map(|t| Rc::new(Value::Nat(*t))).collect(),
                            )),
                            _ => None,
                        };
                        match res {
                            Some(v) => stack.push(Rc::new(v)),
                            None => {
                                stack.push(Rc::new(Value::PartialNativeApp(
                                    builtin.clone(),
                                    vec![arg.clone()],
                                )));
                            }
                        }
                        *idx += 1;
                    }
                    Value::PartialNativeApp(name, args) => {
                        let res = match (
                            name.as_str(),
                            args.iter()
                                .map(|c| &**c)
                                .collect::<Vec<&Value>>()
                                .as_slice(),
                            // args.as_slice(),
                            &*arg,
                        ) {
                            ("Int.+", [Value::Int(a)], Value::Int(b)) => {
                                Value::Int(a.wrapping_add(*b))
                            }
                            ("Int.-", [Value::Int(a)], Value::Int(b)) => {
                                Value::Int(a.wrapping_sub(*b))
                            }
                            ("Int.*", [Value::Int(a)], Value::Int(b)) => Value::Int(a * b),
                            ("Int./", [Value::Int(a)], Value::Int(b)) => Value::Int(a / b),
                            ("Int.<", [Value::Int(a)], Value::Int(b)) => Value::Boolean(*a < *b),
                            ("Int.<=", [Value::Int(a)], Value::Int(b)) => Value::Boolean(*a <= *b),
                            ("Int.>", [Value::Int(a)], Value::Int(b)) => Value::Boolean(*a > *b),
                            ("Int.>=", [Value::Int(a)], Value::Int(b)) => Value::Boolean(*a >= *b),
                            ("Int.==", [Value::Int(a)], Value::Int(b)) => Value::Boolean(*a == *b),
                            ("Int.and", [Value::Int(a)], Value::Int(b)) => Value::Int(a & b),
                            ("Int.or", [Value::Int(a)], Value::Int(b)) => Value::Int(a | b),
                            ("Int.xor", [Value::Int(a)], Value::Int(b)) => Value::Int(a ^ b),
                            ("Int.mod", [Value::Int(a)], Value::Int(b)) => Value::Int(a % b),
                            ("Int.pow", [Value::Int(a)], Value::Nat(b)) => {
                                Value::Int(a.pow(*b as u32))
                            }
                            ("Int.shiftLeft", [Value::Int(a)], Value::Nat(b)) => {
                                Value::Int(a >> *b as u32)
                            }
                            ("Int.shiftRight", [Value::Int(a)], Value::Nat(b)) => {
                                Value::Int(a << *b as u32)
                            }

                            ("Nat.+", [Value::Nat(a)], Value::Nat(b)) => {
                                info!("Nat + {} {}", a, b);
                                Value::Nat(a + b)
                            }
                            ("Nat.*", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a * b),
                            ("Nat./", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a / b),
                            ("Nat.>", [Value::Nat(a)], Value::Nat(b)) => Value::Boolean(*a > *b),
                            ("Nat.>=", [Value::Nat(a)], Value::Nat(b)) => Value::Boolean(*a >= *b),
                            ("Nat.<", [Value::Nat(a)], Value::Nat(b)) => Value::Boolean(*a < *b),
                            ("Nat.<=", [Value::Nat(a)], Value::Nat(b)) => Value::Boolean(*a <= *b),
                            ("Nat.==", [Value::Nat(a)], Value::Nat(b)) => Value::Boolean(*a == *b),
                            ("Nat.and", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a & b),
                            ("Nat.or", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a | b),
                            ("Nat.xor", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a ^ b),
                            ("Nat.mod", [Value::Nat(a)], Value::Nat(b)) => Value::Nat(a % b),
                            ("Nat.pow", [Value::Nat(a)], Value::Nat(b)) => {
                                Value::Nat(a.pow(*b as u32))
                            }
                            ("Nat.shiftLeft", [Value::Nat(a)], Value::Nat(b)) => {
                                Value::Nat(a >> *b as u32)
                            }
                            ("Nat.shiftRight", [Value::Nat(a)], Value::Nat(b)) => {
                                Value::Nat(a << *b as u32)
                            }

                            ("Nat.drop", [Value::Nat(a)], Value::Nat(b)) => {
                                if b >= a {
                                    Value::Nat(0)
                                } else {
                                    Value::Nat(a - b)
                                }
                            }
                            // , ("Nat.drop", 2, DropN (Slot 1) (Slot 0))
                            // , ("Nat.sub", 2, SubN (Slot 1) (Slot 0))
                            // , ("Nat.mod", 2, ModN (Slot 1) (Slot 0))
                            // , ("Nat.pow", 2, PowN (Slot 1) (Slot 0))
                            ("Float.+", [Value::Float(a)], Value::Float(b)) => Value::Float(a + *b),
                            ("Float.-", [Value::Float(a)], Value::Float(b)) => Value::Float(a - *b),
                            ("Float.*", [Value::Float(a)], Value::Float(b)) => Value::Float(a * b),
                            ("Float./", [Value::Float(a)], Value::Float(b)) => Value::Float(a / b),
                            ("Float.<", [Value::Float(a)], Value::Float(b)) => {
                                Value::Boolean(*a < *b)
                            }
                            ("Float.<=", [Value::Float(a)], Value::Float(b)) => {
                                Value::Boolean(*a <= *b)
                            }
                            ("Float.>", [Value::Float(a)], Value::Float(b)) => {
                                Value::Boolean(*a > *b)
                            }
                            ("Float.>=", [Value::Float(a)], Value::Float(b)) => {
                                Value::Boolean(*a >= *b)
                            }
                            ("Float.==", [Value::Float(a)], Value::Float(b)) => {
                                Value::Boolean(*a == *b)
                            }

                            ("Universal.==", [one], two) => Value::Boolean(one == &two),
                            ("Universal.>", [one], two) => Value::Boolean(one > &two),
                            ("Universal.<", [one], two) => Value::Boolean(one < &two),
                            ("Universal.>=", [one], two) => Value::Boolean(one >= &two),
                            ("Universal.<=", [one], two) => Value::Boolean(one <= &two),
                            ("Universal.compare", [one], two) => Value::Int(if one < &two {
                                -1
                            } else if one > &two {
                                1
                            } else {
                                0
                            }),

                            ("Text.++", [Value::Text(a)], Value::Text(b)) => {
                                Value::Text(a.to_owned() + b)
                            }
                            ("Text.==", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a == b),
                            ("Text.!=", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a != b),
                            ("Text.<=", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a <= b),
                            ("Text.>=", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a >= b),
                            ("Text.>", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a > b),
                            ("Text.<", [Value::Text(a)], Value::Text(b)) => Value::Boolean(a < b),
                            // , mk2 "Text.take" atn att (pure . T) (Text.take . fromIntegral)
                            // , mk2 "Text.drop" atn att (pure . T) (Text.drop . fromIntegral)
                            // , mk2 "Text.=="   att att (pure . B) (==)
                            // , mk2 "Text.!="   att att (pure . B) (/=)
                            // , mk2 "Text.<="   att att (pure . B) (<=)
                            // , mk2 "Text.>="   att att (pure . B) (>=)
                            // , mk2 "Text.>"    att att (pure . B) (>)
                            // , mk2 "Text.<"    att att (pure . B) (<)
                            ("List.at", [Value::Nat(a)], Value::Sequence(l)) => {
                                if a < &(l.len() as u64) {
                                    Value::PartialConstructor(
                                        option_ref.clone(),
                                        1,
                                        Vector::from(vec![l[*a as usize].clone()]),
                                    )
                                } else {
                                    Value::Constructor(option_ref.clone(), 0)
                                }
                            }
                            ("List.cons", [value], Value::Sequence(l)) => {
                                let mut l = l.clone();
                                // WOOP
                                l.insert(0, Rc::new((*value).clone()));
                                Value::Sequence(l)
                            }
                            ("List.snoc", [Value::Sequence(l)], _value) => {
                                let mut l = l.clone();
                                l.push_back(arg);
                                Value::Sequence(l)
                            }
                            ("List.take", [Value::Nat(n)], Value::Sequence(l)) => {
                                let l = l.take(*n as usize);
                                Value::Sequence(l)
                            }
                            ("List.drop", [Value::Nat(n)], Value::Sequence(l)) => {
                                if *n as usize >= l.len() {
                                    Value::Sequence(Vector::new())
                                } else {
                                    let l = l.skip(*n as usize);
                                    Value::Sequence(l)
                                }
                            }
                            ("List.++", [Value::Sequence(l0)], Value::Sequence(l1)) => {
                                let mut l = l0.clone();
                                l.extend(l1.clone());
                                Value::Sequence(l)
                            }
                            // , mk2 "Bytes.++"  atbs atbs (pure . Bs) (<>)
                            // , mk2 "Bytes.take" atn atbs (pure . Bs) (\n b -> Bytes.take (fromIntegral n) b)
                            // , mk2 "Bytes.drop" atn atbs (pure . Bs) (\n b -> Bytes.drop (fromIntegral n) b)
                            // , mk2 "Bytes.at" atn atbs pure $ \i bs ->
                            //   IR.maybeToOptional (N . fromIntegral <$> Bytes.at (fromIntegral i) bs)
                            // , mk2 "Float.atan2"     atf atf (pure . F) atan2
                            // , mk2 "Float.logBase"   atf atf (pure . F) logBase

                            // -- Power Functions
                            // , mk2 "Float.pow"       atf atf (pure . F) (**)
                            // -- Float Utils
                            // , mk2 "Float.max"       atf atf (pure . F) max
                            // , mk2 "Float.min"       atf atf (pure . F) min

                            // , mk2 "Debug.watch" att at id (\t v -> putStrLn (Text.unpack t) *> pure v)
                            (a, b, c) => unreachable!(
                                "Native app, we dont have more than two args {} - {:?} - {:?}",
                                a, b, c
                            ),
                        };
                        stack.push(Rc::new(res));
                        *idx += 1;
                    }
                    term => unimplemented!("Call {:?}", term),
                };
                // *idx += 1;
            }
            IR::Seq(num) => {
                let mut v = vec![];
                for _ in 0..*num {
                    // TODO would be nice to ditch the wrappings
                    v.insert(0, stack.pop().unwrap());
                }
                stack.push(Rc::new(Value::Sequence(Vector::from(v))));
                *idx += 1;
            }
            IR::JumpTo(mark) => {
                *idx = *mark;
            }
            IR::Mark(_mark) => {
                // already collected as marks
                *idx += 1;
            }
            IR::If(mark) => match &*stack.pop().unwrap() {
                Value::Boolean(true) => *idx += 1,
                Value::Boolean(false) => {
                    info!("If jumping to {}", mark);
                    *idx = *mark;
                }
                contents => unreachable!("If pop not bool: {:?}", contents),
            },
            IR::IfAndPopStack(mark) => match &*stack.pop().unwrap() {
                Value::Boolean(true) => *idx += 1,
                Value::Boolean(false) => {
                    *idx = *mark;
                    stack.pop_to_mark();
                }
                contents => unreachable!("If pop not bool: {:?}", contents),
            },
            IR::MarkStack => {
                stack.mark();
                *idx += 1;
            }
            IR::ClearStackMark => {
                stack.clear_mark();
                *idx += 1;
            }
            IR::PatternMatch(pattern, has_where) => {
                let value = stack.peek().unwrap();
                if !pattern.matches(&value) {
                    stack.push(Rc::new(Value::Boolean(false)));
                } else {
                    // STOPSHIP: pass Some(value), (value)
                    // so we know not to double-add
                    match pattern.match_(&value) {
                        None => stack.push(Rc::new(Value::Boolean(false))),
                        Some(mut bindings) => {
                            bindings.reverse();
                            if *has_where {
                                for term in &bindings {
                                    stack.push(term.clone());
                                }
                            }
                            for term in bindings {
                                stack.push(term);
                            }
                            // STOPSHIP support primitives
                            stack.push(Rc::new(Value::Boolean(true)))
                        }
                    }
                }
                *idx += 1;
            }
            IR::PatternMatchFail => {
                let value = stack.pop().unwrap();
                match &*value {
                    Value::RequestWithContinuation(req, i, args, back_idx, frames) => {
                        info!("Bubbling up a continuation {:?} # {}", req, i);
                        return Ret::ReRequest(
                            req.clone(),
                            *i,
                            args.clone(),
                            *back_idx,
                            frames.clone(),
                        );
                    }
                    _ => unreachable!("Pattern match failure! {:?}", value),
                }
            }
            IR::PopUpOne => {
                stack.pop_up();
                // stack.0.remove(stack.0.len() - 2);
                *idx += 1;
            }
        }
        Ret::Nothing
    }
}
