use super::ir::{GlobalEnv, IR};
use super::types::*;
use log::info;
use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;

static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

#[derive(Debug, Clone, std::cmp::PartialEq, std::cmp::PartialOrd, Serialize, Deserialize)]
pub enum Source {
    Term(String),
    Fn(usize, String),
}

#[derive(Debug, Clone, std::cmp::PartialEq, std::cmp::PartialOrd, Serialize, Deserialize)]
pub struct Frame {
    source: Source,
    stack: Vec<Term>,
    marks: Vec<usize>,
    handler: Option<usize>,
    return_index: usize,
    bindings: Vec<(Symbol, Term)>,
    binding_marks: Vec<usize>,
}

impl Frame {
    fn new(source: Source, return_index: usize) -> Self {
        Frame {
            source,
            stack: vec![],
            marks: vec![],
            handler: None,
            return_index,
            bindings: vec![],
            binding_marks: vec![],
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
    fn back_again_to_handler(&mut self) -> (usize, Vec<Frame>) {
        let mut frames = vec![];
        self.frames.remove(0); // ignore the current one, it was a clone anyway
        while self.frames[0].handler == None {
            frames.push(self.frames.remove(0));
            if self.frames.len() == 0 {
                unreachable!("Unhandled effect thrown")
            }
        }
        frames.extend(self.frames.clone());
        // frames.push(self.frames[0].clone());
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        (idx, frames)
    }

    // TODO there should be a way to ... get back .. to the thing ... that we wanted ...
    fn back_to_handler(&mut self) -> (usize, Vec<Frame>) {
        let mut frames = vec![];
        while self.frames[0].handler == None {
            frames.push(self.frames.remove(0));
            if self.frames.len() == 0 {
                unreachable!("Unhandled effect thrown")
            }
        }
        frames.extend(self.frames.clone());
        // frames.push(self.frames[0].clone());
        let idx = self.frames[0].handler.unwrap();
        self.frames[0].handler = None;
        (idx, frames)
    }

    fn pop_frame(&mut self) -> (usize, Term) {
        let idx = self.frames[0].return_index;
        let value = self.pop().unwrap();
        self.frames.remove(0);
        info!(
            "{} | <---- Return to idx {} with value {:?}",
            self.frames.len(),
            idx,
            value
        );
        (idx, value)
    }
    // TODO : fn replace_frame
    fn push(&mut self, t: Term) {
        info!("{} | Stack push: {:?}", self.frames.len(), t);
        self.frames[0].stack.push(t);
    }
    fn pop(&mut self) -> Option<Term> {
        let t = self.frames[0].stack.pop();
        info!("{} | Stack pop: {:?}", self.frames.len(), t);
        t
    }
    fn peek(&mut self) -> Option<&Term> {
        let l = self.frames[0].stack.len();
        if l > 0 {
            info!("Stack peek: {:?}", self.frames[0].stack[l - 1]);
            Some(&self.frames[0].stack[l - 1])
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

fn make_marks(cmds: &[IR]) -> HashMap<usize, usize> {
    let mut marks = HashMap::new();
    for i in 0..cmds.len() {
        match &cmds[i] {
            IR::Mark(m) => {
                marks.insert(*m, i);
            }
            _ => (),
        }
    }

    marks
}

#[allow(while_true)]
pub fn eval(env: GlobalEnv, hash: &str) -> Term {
    info!("[- ENV -]");
    for (k, v) in env.terms.iter() {
        info!("] Term {}", k);
        for (n, i) in v.iter().enumerate() {
            info!("({}) {:?}", n, i);
        }
    }
    for (i, v) in env.anon_fns.iter().enumerate() {
        info!("] Fn({}) : {}", i, v.0);
        for (n, i) in v.1.iter().enumerate() {
            info!("({}) {:?}", n, i)
        }
    }

    let mut stack = Stack::new(Source::Term(hash.to_owned()));
    let mut cmds: &Vec<IR> = env.terms.get(hash).unwrap();

    let mut marks = make_marks(&cmds);
    // let mut marks = HashMap::new();
    // for i in 0..cmds.len() {
    //     match &cmds[i] {
    //         IR::Mark(m) => {
    //             marks.insert(*m, i);
    //         }
    //         _ => (),
    //     }
    // }

    let mut idx = 0;

    // let mut n = 0;

    while idx < cmds.len() {
        // n += 1;
        // if n > 100 {
        //     break;
        // }

        let cmd = &cmds[idx];
        info!("----- <{}>    {:?}", idx, cmd);
        match cmd.eval(&mut stack, &mut idx, &marks) {
            Ret::Nothing => (),
            Ret::Handle(mark) => {
                idx += 1;
                let mark_idx = *marks.get(&mark).unwrap();
                stack.frames[0].handler = Some(mark_idx);
                stack.clone_frame(mark_idx);
                stack.frames[0].handler = None;
            }
            Ret::Continue(kidx, frames, arg) => {
                info!(
                    "** CONTINUE ** ({}) {} with {:?} - {:?}",
                    kidx,
                    frames.len(),
                    arg,
                    frames[0].source
                );
                stack.frames = frames;
                for frame in &stack.frames {
                    info!("> {:?}", frame.source);
                }
                idx = kidx;
                stack.push(arg);
                match stack.frames[0].source.clone() {
                    Source::Term(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                }
                marks = make_marks(&cmds);
                // umm
                // ok, so do we need to clone the absolutely whole stack?
                // Or .. is it just "when we go down a level, we need to clone"
                // well let's just clone everything to start, because why not I guess?
            }
            Ret::ReRequest(kind, number, args, final_index, mut frames) => {
                let (nidx, saved_frames) = stack.back_again_to_handler();
                frames.extend(saved_frames);
                idx = nidx;
                stack.push(Term::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    frames,
                ))
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
                let (nidx, saved_frames) = stack.back_to_handler();
                idx = nidx;
                info!(
                    "Jumping back {} frames to {:?}",
                    saved_frames.len(),
                    stack.frames[0].source
                );
                stack.push(Term::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    saved_frames,
                ))
            }
            Ret::FnCall(fnid, bindings, arg) => {
                stack.new_frame(idx, Source::Fn(fnid, env.anon_fns[fnid].0.clone()));
                info!("^ fn call with {:?}", arg);
                stack.frames[0].bindings = bindings;
                stack.frames[0].stack.push(arg);
                cmds = &env.anon_fns[fnid].1;
                marks = make_marks(&cmds);
                // // cmds = env.anon_fns.get(&hash.to_string()).unwrap();
                // info!("[Fn Instructions - {}]", fnid);
                // for cmd in cmds {
                //     info!("{:?}", cmd);
                // }
                idx = 0;
            }
            Ret::Term(hash) => {
                // info!("Jumping to {:?}", hash);
                stack.new_frame(idx, Source::Term(hash.to_string()));
                cmds = env.terms.get(&hash.to_string()).unwrap();
                marks = make_marks(&cmds);
                // info!("[Term Instructions]");
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
                    Source::Term(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };
                marks = make_marks(&cmds);
            }
        }
        // Multi-pop
        while idx >= cmds.len() {
            if stack.frames.len() > 1 {
                // info!("<<-- jump down");
                let (idx1, value) = stack.pop_frame();
                idx = idx1;
                // stack.pop_frame();
                // stack.frames.remove(0);
                match stack.frames[0].source.clone() {
                    Source::Term(hash) => {
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
                marks = make_marks(&cmds);
            } else {
                info!("Got only one frame left, and idx is larger than the cmds len");
                break;
            }
        }
    }

    println!("All done I guess; {} {}", idx, cmds.len());

    // while idx < cmds.len() {
    //     let cmd = &cmds[idx];
    //     match cmd.eval(
    //         &env,
    //         &mut bindings,
    //         &mut binding_marks,
    //         &mut stack,
    //         &mut idx,
    //         &marks,
    //     ) {
    //         None => (),
    //         Some(hash) => {
    //             // JUMPPP
    //             // cmds =
    //         }
    //     }
    // }
    info!("Final stack: {:?}", stack);
    stack.pop().unwrap()
}

enum Ret {
    FnCall(usize, Vec<(Symbol, Term)>, Term),
    Term(Hash),
    Nothing,
    Request(Reference, usize, Vec<Term>),
    ReRequest(Reference, usize, Vec<Term>, usize, Vec<Frame>),
    Handle(usize),
    HandlePure,
    Continue(usize, Vec<Frame>, Term),
}

impl IR {
    fn eval(
        &self,
        // env: &GlobalEnv,
        stack: &mut Stack,
        idx: &mut usize,
        marks: &HashMap<usize, usize>,
    ) -> Ret {
        match self {
            IR::MarkBindings => {
                let ln = stack.frames[0].bindings.len();
                stack.frames[0].binding_marks.push(ln);
                *idx += 1;
            }
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
                stack.push(Term::RequestPure(Box::new(v)));
                // *idx += 1;
                return Ret::HandlePure;
                // return Ret::RequestPure(v);
            }
            IR::PopBindings => {
                let mark = stack.frames[0].binding_marks.pop().unwrap();
                // lol there's probably a better way
                while stack.frames[0].bindings.len() > mark {
                    stack.frames[0].bindings.remove(0);
                }
                *idx += 1;
            }
            // IR::Value(Term::Request(constructor, num))
            IR::Value(term) => {
                match term {
                    Term::Request(a, b) => {
                        *idx += 1;
                        return Ret::Request(a.clone(), *b, vec![]);
                    }
                    Term::RequestWithArgs(a, b, n, args) if *n == args.len() => {
                        *idx += 1;
                        return Ret::Request(a.clone(), *b, args.clone());
                    }
                    Term::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                        // Jump!
                        *idx += 1;
                        return Ret::Term(hash.clone());
                        // let value = env.terms.get(&hash.to_string()).clone();
                        // stack.push_frame(idx, hash.to_string());
                        // *idx = 0;
                        // cmds = value;
                        // JUMP!
                    }
                    _ => {
                        stack.push(term.clone());
                        *idx += 1;
                    }
                };
            }
            // IR::Reference()
            // IR::Ref(hash) => {
            //     stack.push(Term::Ref(Reference::))
            //     // let res = env.load(&hash.to_string());
            // }
            // IR::Builtin(name) => {
            //     stack.push(Term::Ref(Reference::Builtin(name.clone())));
            //     *idx += 1;
            // }
            IR::PushSym(symbol) => {
                let v = match stack.frames[0]
                    .bindings
                    .iter()
                    .find(|(k, _)| symbol.text == k.text)
                {
                    None => unreachable!("Vbl not found {}", symbol.text),
                    Some((_, v)) => v.clone(),
                };
                stack.push(v);
                *idx += 1;
            }
            IR::PopAndName(symbol) => {
                let v = stack.pop().unwrap();
                stack.frames[0].bindings.insert(0, (symbol.clone(), v));
                *idx += 1;
            }
            IR::Fn(i) => {
                stack.push(Term::PartialFnBody(*i, stack.frames[0].bindings.clone()));
                *idx += 1;
            }
            IR::Cycle(names) => {
                let mut mutuals = vec![];
                let mut items = vec![];
                for name in names {
                    let v = stack.pop().unwrap();
                    match v {
                        Term::PartialFnBody(fnint, bindings) => {
                            mutuals.push((Symbol::new(name.clone()), fnint));
                            items.push((name, fnint, bindings));
                        }
                        _ => {
                            stack.frames[0]
                                .bindings
                                .push((Symbol::new(name.clone()), v));
                        }
                    }
                }
                for (name, fnint, bindings) in items {
                    stack.frames[0].bindings.push((
                        Symbol::new(name.clone()),
                        Term::CycleFnBody(fnint, bindings, mutuals.clone()),
                    ))
                }

                // stack.push(Term::CycleFnBody(
                //     *i,
                //     stack.frames[0].bindings.clone(),
                //     mutuals.clone(),
                // ));
                *idx += 1;
            }
            // IR::CycleFn(i, mutuals) => {
            //     stack.push(Term::CycleFnBody(
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
                match f {
                    Term::Continuation(kidx, frames) => return Ret::Continue(kidx, frames, arg),
                    Term::RequestWithArgs(r, i, n, mut args) => {
                        *idx += 1;
                        args.push(arg);
                        if args.len() == n {
                            return Ret::Request(r, i, args);
                        }
                        info!("- request - {:?} - {}", args, n);
                        stack.push(Term::RequestWithArgs(r, i, n, args));
                    }
                    Term::Constructor(r, u) => {
                        stack.push(Term::PartialConstructor(r, u, vec![arg]));
                        *idx += 1;
                    }
                    Term::PartialConstructor(r, u, c) => {
                        let mut c = c.clone();
                        c.push(arg);
                        stack.push(Term::PartialConstructor(r, u, c));
                        *idx += 1;
                    }
                    Term::CycleFnBody(fnint, mut bindings, mutuals) => {
                        *idx += 1;
                        let copy = bindings.clone();
                        for (k, v) in &mutuals {
                            bindings.push((
                                k.clone(),
                                Term::CycleFnBody(*v, copy.clone(), mutuals.clone()),
                            ))
                        }
                        return Ret::FnCall(fnint, bindings, arg);
                    }
                    Term::PartialFnBody(fnint, bindings) => {
                        *idx += 1;
                        return Ret::FnCall(fnint, bindings, arg);
                    }
                    Term::Ref(Reference::Builtin(builtin)) => {
                        let res = match (builtin.as_str(), &arg) {
                            ("Int.increment", Term::Int(i)) => Some(Term::Int(i + 1)),
                            ("Int.negate", Term::Int(i)) => Some(Term::Int(-i)),
                            ("Int.isEven", Term::Int(i)) => Some(Term::Boolean(i % 2 == 0)),
                            ("Int.isOdd", Term::Int(i)) => Some(Term::Boolean(i % 2 == 1)),
                            ("Nat.increment", Term::Nat(i)) => Some(Term::Nat(i + 1)),
                            ("Nat.isEvent", Term::Nat(i)) => Some(Term::Boolean(i % 2 == 0)),
                            ("Nat.isOdd", Term::Nat(i)) => Some(Term::Boolean(i % 2 == 1)),
                            ("Nat.toInt", Term::Nat(i)) => Some(Term::Int(*i as i64)),
                            ("Nat.toText", Term::Nat(i)) => Some(Term::Text(i.to_string())),
                            ("Boolean.not", Term::Boolean(i)) => Some(Term::Boolean(!i)),
                            ("List.size", Term::Sequence(s)) => Some(Term::Nat(s.len() as u64)),
                            ("Text.size", Term::Text(t)) => Some(Term::Nat(t.len() as u64)),
                            ("Bytes.size", Term::Bytes(t)) => Some(Term::Nat(t.len() as u64)),
                            ("Bytes.toList", Term::Bytes(t)) => Some(Term::Sequence(
                                t.iter().map(|t| Box::new(ABT::Tm(Term::Nat(*t)))).collect(),
                            )),
                            _ => None,
                        };
                        match res {
                            Some(v) => stack.push(v),
                            None => {
                                stack.push(Term::PartialNativeApp(builtin, vec![arg.clone()]));
                            }
                        }
                        *idx += 1;
                    }
                    Term::PartialNativeApp(name, args) => {
                        let res = match (name.as_str(), args.as_slice(), &arg) {
                            ("Int.+", [Term::Int(a)], Term::Int(b)) => {
                                Term::Int(a.wrapping_add(*b))
                            }
                            ("Int.-", [Term::Int(a)], Term::Int(b)) => {
                                Term::Int(a.wrapping_sub(*b))
                            }
                            ("Int.*", [Term::Int(a)], Term::Int(b)) => Term::Int(a * b),
                            ("Int./", [Term::Int(a)], Term::Int(b)) => Term::Int(a / b),
                            ("Int.<", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a < *b),
                            ("Int.<=", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a <= *b),
                            ("Int.>", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a > *b),
                            ("Int.>=", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a >= *b),
                            ("Int.==", [Term::Int(a)], Term::Int(b)) => Term::Boolean(*a == *b),
                            ("Int.and", [Term::Int(a)], Term::Int(b)) => Term::Int(a & b),
                            ("Int.or", [Term::Int(a)], Term::Int(b)) => Term::Int(a | b),
                            ("Int.xor", [Term::Int(a)], Term::Int(b)) => Term::Int(a ^ b),
                            ("Int.mod", [Term::Int(a)], Term::Int(b)) => Term::Int(a % b),
                            ("Int.pow", [Term::Int(a)], Term::Nat(b)) => {
                                Term::Int(a.pow(*b as u32))
                            }
                            ("Int.shiftLeft", [Term::Int(a)], Term::Nat(b)) => {
                                Term::Int(a >> *b as u32)
                            }
                            ("Int.shiftRight", [Term::Int(a)], Term::Nat(b)) => {
                                Term::Int(a << *b as u32)
                            }

                            ("Nat.+", [Term::Nat(a)], Term::Nat(b)) => {
                                info!("Nat + {} {}", a, b);
                                Term::Nat(a + b)
                            }
                            ("Nat.*", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a * b),
                            ("Nat./", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a / b),
                            ("Nat.>", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a > *b),
                            ("Nat.>=", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a >= *b),
                            ("Nat.<", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a < *b),
                            ("Nat.<=", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a <= *b),
                            ("Nat.==", [Term::Nat(a)], Term::Nat(b)) => Term::Boolean(*a == *b),
                            ("Nat.and", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a & b),
                            ("Nat.or", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a | b),
                            ("Nat.xor", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a ^ b),
                            ("Nat.mod", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a % b),
                            ("Nat.pow", [Term::Nat(a)], Term::Nat(b)) => {
                                Term::Nat(a.pow(*b as u32))
                            }
                            ("Nat.shiftLeft", [Term::Nat(a)], Term::Nat(b)) => {
                                Term::Nat(a >> *b as u32)
                            }
                            ("Nat.shiftRight", [Term::Nat(a)], Term::Nat(b)) => {
                                Term::Nat(a << *b as u32)
                            }

                            ("Nat.drop", [Term::Nat(a)], Term::Nat(b)) => {
                                if b >= a {
                                    Term::Nat(0)
                                } else {
                                    Term::Nat(a - b)
                                }
                            }
                            // , ("Nat.drop", 2, DropN (Slot 1) (Slot 0))
                            // , ("Nat.sub", 2, SubN (Slot 1) (Slot 0))
                            // , ("Nat.mod", 2, ModN (Slot 1) (Slot 0))
                            // , ("Nat.pow", 2, PowN (Slot 1) (Slot 0))
                            ("Float.+", [Term::Float(a)], Term::Float(b)) => Term::Float(a + b),
                            ("Float.-", [Term::Float(a)], Term::Float(b)) => Term::Float(a - b),
                            ("Float.*", [Term::Float(a)], Term::Float(b)) => Term::Float(a * b),
                            ("Float./", [Term::Float(a)], Term::Float(b)) => Term::Float(a / b),
                            ("Float.<", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a < *b),
                            ("Float.<=", [Term::Float(a)], Term::Float(b)) => {
                                Term::Boolean(*a <= *b)
                            }
                            ("Float.>", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a > *b),
                            ("Float.>=", [Term::Float(a)], Term::Float(b)) => {
                                Term::Boolean(*a >= *b)
                            }
                            ("Float.==", [Term::Float(a)], Term::Float(b)) => {
                                Term::Boolean(*a == *b)
                            }

                            ("Universal.==", [one], two) => Term::Boolean(one == two),
                            ("Universal.>", [one], two) => Term::Boolean(one > two),
                            ("Universal.<", [one], two) => Term::Boolean(one < two),
                            ("Universal.>=", [one], two) => Term::Boolean(one >= two),
                            ("Universal.<=", [one], two) => Term::Boolean(one <= two),
                            ("Universal.compare", [one], two) => Term::Int(if one < two {
                                -1
                            } else if one > two {
                                1
                            } else {
                                0
                            }),

                            ("Text.++", [Term::Text(a)], Term::Text(b)) => {
                                Term::Text(a.to_owned() + b)
                            }
                            ("Text.==", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a == b),
                            ("Text.!=", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a != b),
                            ("Text.<=", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a <= b),
                            ("Text.>=", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a >= b),
                            ("Text.>", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a > b),
                            ("Text.<", [Term::Text(a)], Term::Text(b)) => Term::Boolean(a < b),
                            // , mk2 "Text.take" atn att (pure . T) (Text.take . fromIntegral)
                            // , mk2 "Text.drop" atn att (pure . T) (Text.drop . fromIntegral)
                            // , mk2 "Text.=="   att att (pure . B) (==)
                            // , mk2 "Text.!="   att att (pure . B) (/=)
                            // , mk2 "Text.<="   att att (pure . B) (<=)
                            // , mk2 "Text.>="   att att (pure . B) (>=)
                            // , mk2 "Text.>"    att att (pure . B) (>)
                            // , mk2 "Text.<"    att att (pure . B) (<)
                            ("List.at", [Term::Nat(a)], Term::Sequence(l)) => {
                                if a < &(l.len() as u64) {
                                    Term::PartialConstructor(
                                        Reference::from_hash(OPTION_HASH),
                                        1,
                                        vec![match &*l[*a as usize] {
                                            ABT::Tm(term) => term.clone(),
                                            _ => unreachable!(),
                                        }],
                                    )
                                } else {
                                    Term::Constructor(Reference::from_hash(OPTION_HASH), 0)
                                }
                            }
                            ("List.cons", [value], Term::Sequence(l)) => {
                                let mut l = l.clone();
                                l.insert(0, Box::new(value.clone().into()));
                                Term::Sequence(l)
                            }
                            ("List.snoc", [Term::Sequence(l)], value) => {
                                let mut l = l.clone();
                                l.push(Box::new(value.clone().into()));
                                Term::Sequence(l)
                            }
                            ("List.take", [Term::Nat(n)], Term::Sequence(l)) => {
                                let l = l[0..*n as usize].to_vec();
                                Term::Sequence(l)
                            }
                            ("List.drop", [Term::Nat(n)], Term::Sequence(l)) => {
                                let l = l[*n as usize..].to_vec();
                                Term::Sequence(l)
                            }
                            ("List.++", [Term::Sequence(l0)], Term::Sequence(l1)) => {
                                let mut l = l0.clone();
                                l.extend(l1.clone());
                                Term::Sequence(l)
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
                        stack.push(res);
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
                    v.insert(0, Box::new(stack.pop().unwrap().into()))
                }
                stack.push(Term::Sequence(v));
                *idx += 1;
            }
            IR::JumpTo(mark) => {
                *idx = *marks.get(mark).unwrap();
            }
            IR::Mark(_mark) => {
                // already collected as marks
                *idx += 1;
            }
            IR::If(mark) => match stack.pop() {
                None => unreachable!("If pop"),
                Some(Term::Boolean(true)) => *idx += 1,
                Some(Term::Boolean(false)) => {
                    info!("If jumping to {}", mark);
                    *idx = *marks.get(mark).unwrap();
                }
                Some(contents) => unreachable!("If pop not bool: {:?}", contents),
            },
            IR::IfAndPopStack(mark) => match stack.pop() {
                None => unreachable!("If pop"),
                Some(Term::Boolean(true)) => *idx += 1,
                Some(Term::Boolean(false)) => {
                    *idx = *marks.get(mark).unwrap();
                    stack.pop_to_mark();
                }
                Some(contents) => unreachable!("If pop not bool: {:?}", contents),
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
                match pattern.match_(&value) {
                    None => stack.push(Term::Boolean(false)),
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
                        stack.push(Term::Boolean(true))
                    }
                }
                *idx += 1;
            }
            IR::PatternMatchFail => {
                let value = stack.pop().unwrap();
                match value {
                    Term::RequestWithContinuation(req, i, args, back_idx, frames) => {
                        return Ret::ReRequest(req, i, args, back_idx, frames)
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
