use super::frame::Frame;
use super::ir::IR;
use super::stack::Stack;
use super::types::*;
use im::Vector;
use log::info;
use std::rc::Rc;

pub enum Ret {
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
    pub fn eval(&self, option_ref: &Reference, stack: &mut Stack, idx: &mut usize) -> Ret {
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
                return Ret::HandlePure;
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
                        *idx += 1;
                        return Ret::Value(hash.clone());
                    }
                    _ => {
                        stack.push(Rc::new(term.clone()));
                        *idx += 1;
                    }
                };
            }
            IR::PushSym(symbol, usage) => {
                let v = stack.get_vbl(symbol, *usage);
                stack.push(v);
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
                info!("Binding free vbls {:?}", free_vbls);
                let bound: Vec<(Symbol, usize, Rc<Value>)> = free_vbls
                    .iter()
                    .enumerate()
                    .map(|(i, (sym, external, internal, cycle))| {
                        (sym.with_unique(i), *internal, {
                            if *cycle {
                                Rc::new(Value::CycleBlank(sym.unique))
                            } else {
                                stack.get_vbl(sym, *external)
                            }
                        })
                    })
                    .collect();
                stack.push(Rc::new(Value::PartialFnBody(*i, bound)));
                *idx += 1;
            }
            IR::Cycle(names) => {
                let mut mutuals = vec![];
                let mut items = vec![];

                // let mut all_bindings = vec![];

                for (name, uses) in names {
                    let v = stack.pop().unwrap();
                    match &*v {
                        Value::PartialFnBody(fnint, bindings) => {
                            mutuals.push((name.clone(), *uses, *fnint, bindings.clone()));
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
                *idx += 1;
            }
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
                        info!("calling CycleFnBody, {} bindings", bindings.len());
                        let mut bindings = bindings.clone();
                        for binding in bindings.iter_mut() {
                            match &*binding.2 {
                                Value::CycleBlank(u) => {
                                    let (k, uses, fnid, sub_bindings) =
                                        mutuals.iter().find(|m| m.0.unique == *u).unwrap();
                                    info!("Found cycle blank({}) - subbing in {:?}", u, k);

                                    binding.1 = *uses;
                                    binding.2 = Rc::new(Value::CycleFnBody(
                                        *fnid,
                                        sub_bindings.clone(),
                                        mutuals.clone(),
                                    ));
                                }
                                _ => (),
                            }
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
