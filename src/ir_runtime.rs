use super::ir::{GlobalEnv, IREnv, IR};
use super::types::*;
use log::info;
use std::collections::HashMap;

static option_hash: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

#[derive(Debug)]
struct Frame {
    source: String,
    stack: Vec<Term>,
    marks: Vec<usize>,
    return_index: usize,
    bindings: Vec<(Symbol, Term)>,
    binding_marks: Vec<usize>,
}

impl Frame {
    fn new(source: String, return_index: usize) -> Self {
        Frame {
            source,
            stack: vec![],
            marks: vec![],
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
    fn new(source: String) -> Self {
        Stack {
            frames: vec![Frame::new(source, 0)],
        }
    }
    fn new_frame(&mut self, return_index: usize, source: String) {
        self.frames.insert(0, Frame::new(source, return_index))
    }
    // TODO
    // fn replace_frame
    fn push(&mut self, t: Term) {
        println!("Stack push: {:?}", t);
        self.frames[0].stack.push(t);
    }
    fn pop(&mut self) -> Option<Term> {
        let t = self.frames[0].stack.pop();
        println!("Stack pop: {:?}", t);
        t
    }
    fn peek(&mut self) -> Option<&Term> {
        let l = self.frames[0].stack.len();
        if l > 0 {
            println!("Stack peek: {:?}", self.frames[0].stack[l - 1]);
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

pub fn eval(env: GlobalEnv, hash: &str) -> Term {
    let mut stack = Stack::new(hash.to_owned());
    let mut cmds: Vec<IR> = env.terms.get(hash).unwrap().clone();

    for c in &cmds {
        println!("{:?}", c);
    }

    let mut marks = HashMap::new();
    for i in 0..cmds.len() {
        match &cmds[i] {
            IR::Mark(m) => {
                marks.insert(*m, i);
            }
            _ => (),
        }
    }

    let mut idx = 0;

    while true {
        let cmd = &cmds[idx];
        match cmd.eval(&env, &mut stack, &mut idx, &marks) {
            None => (),
            Some(hash) => {
                // JUMPPP
                // cmds =
            }
        }
        if idx >= cmds.len() {
            break;
        }
    }

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
    println!("Final stack: {:?}", stack);
    stack.pop().unwrap()
}

impl IR {
    fn eval(
        &self,
        env: &GlobalEnv,
        stack: &mut Stack,
        idx: &mut usize,
        marks: &HashMap<usize, usize>,
    ) -> Option<Hash> {
        match self {
            IR::MarkBindings => {
                let ln = stack.frames[0].bindings.len();
                stack.frames[0].binding_marks.push(ln);
                *idx += 1;
            }
            IR::PopBindings => {
                let mark = stack.frames[0].binding_marks.pop().unwrap();
                // lol there's probably a better way
                while stack.frames[0].bindings.len() > mark {
                    stack.frames[0].bindings.remove(0);
                }
                *idx += 1;
            }
            IR::Value(term) => {
                match term {
                    Term::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                        // Jump!
                        return Some(hash.clone());
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
                match stack.frames[0]
                    .bindings
                    .iter()
                    .find(|(k, _)| symbol.text == k.text)
                {
                    None => unreachable!("Vbl not found {}", symbol.text),
                    Some((_, v)) => {
                        stack.push(v.clone());
                        *idx += 1;
                    }
                }
            }
            IR::PopAndName(symbol) => {
                let v = stack.pop().unwrap();
                stack.frames[0].bindings.insert(0, (symbol.clone(), v));
                *idx += 1;
            }
            IR::Call => {
                let arg = stack.pop().unwrap();
                let f = stack.pop().unwrap();
                match f {
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
                    }
                    Term::PartialNativeApp(name, args) => {
                        let res = match (name.as_str(), args.as_slice(), &arg) {
                            ("Int.+", [Term::Int(a)], Term::Int(b)) => Term::Int(a + b),
                            ("Int.-", [Term::Int(a)], Term::Int(b)) => Term::Int(a - b),
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
                                        Reference::from_hash(option_hash),
                                        1,
                                        vec![match &*l[*a as usize] {
                                            ABT::Tm(term) => term.clone(),
                                            _ => unreachable!(),
                                        }],
                                    )
                                // Term::App(
                                //     Box::new(
                                //         Term::Constructor(Reference::from_hash(option_hash), 1)
                                //             .into(),
                                //     ),
                                //     Box::new(l[*a as usize].eval(env, stack).into()),
                                // )
                                } else {
                                    Term::Constructor(Reference::from_hash(option_hash), 0)
                                }
                            }
                            // , mk2 "List.at" atn ats (pure . IR.maybeToOptional)
                            //   $ Sequence.lookup
                            //   . fromIntegral
                            // , mk2 "List.cons" at  ats (pure . Sequence) (Sequence.<|)
                            // , mk2 "List.snoc" ats at  (pure . Sequence) (Sequence.|>)
                            // , mk2 "List.take" atn ats (pure . Sequence) (Sequence.take . fromIntegral)
                            // , mk2 "List.drop" atn ats (pure . Sequence) (Sequence.drop . fromIntegral)
                            // , mk2 "List.++"   ats ats (pure . Sequence) (<>)
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
                    }
                    _ => unimplemented!(),
                };
                *idx += 1;
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
            IR::PatternMatchFail => unreachable!("Pattern match failure!"),
            IR::PopUpOne => {
                stack.pop_up();
                // stack.0.remove(stack.0.len() - 2);
                *idx += 1;
            }
        }
        None
    }
}
