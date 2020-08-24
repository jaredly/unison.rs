use super::env::*;
use super::types::*;
use log::info;

// trait Eval
/*

Ok, what's the simplest way of doing this that I can imagine?

The thing that seems like it'll be hardest to do is the Effect system.
Because we need multi-shot continuations.
Which means I think I need stack frames.

but first, I feel like it makes sense to just try to get a bare-bones something working,
and then I can think about stack frames. Right?

after all, the stack is a Vec of Map's, right? Mapping from variable names to values?
hrmmm but not completely, because I also need to represent the exact place where we'll be returning to.
And being able to "pick up where we left off".

What does this mean?

So, one way to think of "where are we going" is to assign each "moment" in a given term an index ...
... and say "this is where we're going back to".
So like, .. the moments would be .. yeah, it would be a local stack of bindings, and an index of where we are at ..
and then "resuming" a continuation would mean going to the given term, walking the tree until we get to the index,
and resuming.
I like that.

*/
pub trait Eval {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term;
}

static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

impl From<Term> for ABT<Term> {
    fn from(term: Term) -> Self {
        // pub fn from_term(term: Term) -> Box<Self> {
        ABT::Tm(term)
        // }
    }
}

impl ABT<Term> {
    pub fn eval_with_bindings(&self, env: &mut Env, stack: &Stack, bindings: Vec<Term>) -> Term {
        // let kvs = vec![];
        let mut body = self;
        let mut new_stack = stack.clone();
        info!("<eval w/ bindings>");
        for binding in bindings.into_iter() {
            match body {
                ABT::Abs(sym, inner) => {
                    new_stack.set(sym.text.clone(), binding);
                    body = inner;
                }
                _ => unreachable!("NOP"),
            }
        }
        body.eval(env, &new_stack)
    }
}

fn unroll_cycle(
    inner: &ABT<Term>,
    names: &mut Vec<String>,
) -> (Vec<Box<ABT<Term>>>, Box<ABT<Term>>) {
    match inner {
        ABT::Abs(sym, inner) => {
            names.push(sym.text.clone());
            match &**inner {
                ABT::Tm(Term::LetRec(_, things, body)) => (things.clone(), body.clone()),
                _ => unroll_cycle(inner, names),
            }
        }
        _ => unreachable!("Cycle not abs"),
    }
}
/*

mnetal model for recursion-

you have a fn
thta fn has a scope that it came iwth
that scope needs to include itself
so, when you execute the fn
you first copy it into itself, and then go to town.

yeah sounds easy enough.

*/

impl Eval for ABT<Term> {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term {
        match self {
            ABT::Var(sym) => stack.lookup(&sym.text),
            ABT::Cycle(inner) => {
                let mut names = vec![];
                let (values, body) = unroll_cycle(inner, &mut names);
                let mut new_stack = stack.clone();
                let mut cycle_bindings = vec![];
                info!("CYCLE {:?}", names);
                for i in 0..names.len() {
                    let f = values[i].eval(env, stack);
                    cycle_bindings.push((names[i].clone(), f));
                }
                for (k, v) in &cycle_bindings {
                    new_stack.set(
                        k.clone(),
                        Term::Cycle(Box::new(v.clone()), cycle_bindings.clone()),
                    );
                }
                body.eval(env, &new_stack)
            }
            ABT::Abs(_sym, _inner) => unreachable!("Raw abs {}", stack.0[0].term),
            ABT::Tm(inner) => inner.eval(env, stack),
        }
    }
}

// Types of things we'll be dealing with:
// Terms as they are
// Partially applied function calls
// Builtins (which can be functions, mostly are probably?) - I guess a Ref() can be what we do for builtins

impl Eval for Term {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term {
        match self {
            Term::Int(_)
            | Term::Nat(_)
            | Term::Float(_)
            | Term::Boolean(_)
            | Term::Text(_)
            | Term::Char(_)
            | Term::TermLink(_)
            | Term::TypeLink(_)
            | Term::PartialConstructor(_, _, _)
            | Term::Blank => self.clone(),
            Term::Let(_, value, contents) => match &**contents {
                ABT::Abs(name, contents) => {
                    let value = value.eval(env, stack);
                    contents.eval(env, &stack.with(name.text.clone(), value))
                }
                _ => unreachable!(),
            },
            Term::Ref(Reference::DerivedId(Id(hash, _, _))) => env
                .load(&hash.to_string())
                .eval(env, &stack.with_frame(hash.to_string())),
            Term::Ref(Reference::Builtin(contents)) => {
                match contents.as_str() {
                    "Text.empty" => Term::Text("".to_string()),
                    "Sequence.empty" => Term::Sequence(vec![]),
                    // "Bytes.empty" => Term::BYtes(""),
                    _ => self.clone(),
                }
            }

            Term::Constructor(_, _) => self.clone(),
            Term::Request(_, _) => unimplemented!("Request {:?}", self),
            Term::Handle(_contents, _handler) => unimplemented!("Handle {:?}", self),
            Term::LetRec(_, _values, _body) => unimplemented!("LetRec {:?}", self),
            Term::Match(term, arms) => {
                let term = term.eval(env, stack);
                for MatchCase(pattern, where_term, body) in arms {
                    match pattern.matches(&term, where_term, env, &stack) {
                        None => (),
                        Some(bindings) => {
                            return (*body).eval_with_bindings(env, stack, bindings);
                        }
                    }
                }
                unreachable!(
                    "Nothing matched\n - value: {:?}\n - arms: {:?} - {}",
                    term, arms, stack.0[0].term
                );
            }

            Term::Ann(term, _type) => term.eval(env, stack),
            Term::Sequence(contents) => {
                let mut res = vec![];
                for item in contents.iter() {
                    res.push(Box::new(ABT::Tm(item.eval(env, stack))))
                }
                Term::Sequence(res)
            }
            Term::If(one, two, three) => match one.eval(env, stack) {
                Term::Boolean(true) => two.eval(env, stack),
                Term::Boolean(false) => three.eval(env, stack),
                _ => unreachable!("If with not a bool {:?}", self),
            },
            Term::And(one, two) => match (one.eval(env, stack), two.eval(env, stack)) {
                (Term::Boolean(a), Term::Boolean(b)) => Term::Boolean(a && b),
                (a, b) => unreachable!("and not bool {:?} and {:?}", a, b),
            },
            Term::Or(one, two) => match (one.eval(env, stack), two.eval(env, stack)) {
                (Term::Boolean(a), Term::Boolean(b)) => Term::Boolean(a || b),
                (a, b) => unreachable!("or not bool {:?} and {:?}", a, b),
            },
            // Term::Lam(contents) => Term::Lam(Box::new(ABT::Tm(contents.eval(env, stack)))),
            Term::Lam(contents) => Term::ScopedFunction(
                contents.clone(),
                stack.0[0].term.clone(),
                stack.0[0].bindings.clone(),
            ),
            // Term::LetRec(
            Term::App(one, two) => {
                let one = one.eval(env, stack);
                match one {
                    Term::Constructor(r, u) => {
                        Term::PartialConstructor(r, u, vec![two.eval(env, stack)])
                    }
                    Term::PartialConstructor(r, u, c) => {
                        let mut c = c.clone();
                        c.push(two.eval(env, stack));
                        Term::PartialConstructor(r, u, c)
                    }
                    Term::PartialNativeApp(name, body) => {
                        match (name.as_str(), body.as_slice(), &two.eval(env, stack)) {
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
                                        Reference::from_hash(OPTION_HASH),
                                        1,
                                        vec![l[*a as usize].eval(env, stack)],
                                    )
                                // Term::App(
                                //     Box::new(
                                //         Term::Constructor(Reference::from_hash(option_hash), 1)
                                //             .into(),
                                //     ),
                                //     Box::new(l[*a as usize].eval(env, stack).into()),
                                // )
                                } else {
                                    Term::Constructor(Reference::from_hash(OPTION_HASH), 0)
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
                        }
                    }

                    Term::Ref(Reference::Builtin(builtin)) => {
                        match (builtin.as_str(), two.eval(env, stack)) {
                            ("Int.increment", Term::Int(i)) => Term::Int(i + 1),
                            ("Int.negate", Term::Int(i)) => Term::Int(-i),
                            ("Int.isEven", Term::Int(i)) => Term::Boolean(i % 2 == 0),
                            ("Int.isOdd", Term::Int(i)) => Term::Boolean(i % 2 == 1),
                            ("Nat.increment", Term::Nat(i)) => Term::Nat(i + 1),
                            ("Nat.isEvent", Term::Nat(i)) => Term::Boolean(i % 2 == 0),
                            ("Nat.isOdd", Term::Nat(i)) => Term::Boolean(i % 2 == 1),
                            ("Nat.toInt", Term::Nat(i)) => Term::Int(i as i64),
                            ("Boolean.not", Term::Boolean(i)) => Term::Boolean(!i),
                            ("List.size", Term::Sequence(s)) => Term::Nat(s.len() as u64),
                            ("Text.size", Term::Text(t)) => Term::Nat(t.len() as u64),
                            ("Bytes.size", Term::Bytes(t)) => Term::Nat(t.len() as u64),
                            ("Bytes.toList", Term::Bytes(t)) => Term::Sequence(
                                t.iter().map(|t| Box::new(ABT::Tm(Term::Nat(*t)))).collect(),
                            ),
                            // , mk1 "Bytes.fromList" ats (pure . Bs) (\s ->
                            //     Bytes.fromByteString (BS.pack [ fromIntegral n | N n <- toList s]))

                            // , mk1 "Bytes.toList" atbs (pure . Sequence)
                            //     (\bs -> Sequence.fromList [ N (fromIntegral n) | n <- Bytes.toWord8s bs ])
                            // , mk1 "Bytes.flatten" atbs (pure . Bs) Bytes.flatten

                            // -- Trigonometric functions
                            // , mk1 "Float.acos"          atf (pure . F) acos
                            // , mk1 "Float.asin"          atf (pure . F) asin
                            // , mk1 "Float.atan"          atf (pure . F) atan

                            // , mk1 "Float.cos"           atf (pure . F) cos
                            // , mk1 "Float.sin"           atf (pure . F) sin
                            // , mk1 "Float.tan"           atf (pure . F) tan

                            // -- Hyperbolic functions
                            // , mk1 "Float.acosh"         atf (pure . F) acosh
                            // , mk1 "Float.asinh"         atf (pure . F) asinh
                            // , mk1 "Float.atanh"         atf (pure . F) atanh
                            // , mk1 "Float.cosh"          atf (pure . F) cosh
                            // , mk1 "Float.sinh"          atf (pure . F) sinh
                            // , mk1 "Float.tanh"          atf (pure . F) tanh

                            // -- Exponential functions
                            // , mk1 "Float.exp"           atf (pure . F) exp
                            // , mk1 "Float.log"           atf (pure . F) log

                            // , mk1 "Float.sqrt"          atf (pure . F) sqrt

                            // -- Rounding and Remainder Functions
                            // , mk1 "Float.ceiling"       atf (pure . I) ceiling
                            // , mk1 "Float.floor"         atf (pure . I) floor
                            // , mk1 "Float.round"         atf (pure . I) round
                            // , mk1 "Float.truncate"      atf (pure . I) truncate

                            // , mk1 "Nat.toText" atn (pure . T) (Text.pack . show)
                            // , mk1 "Nat.fromText" att (pure . IR.maybeToOptional . fmap N) (
                            //     (\x -> readMaybe x :: Maybe Word64) . Text.unpack)
                            // , mk1 "Nat.toFloat" atn (pure . F) fromIntegral

                            // , mk1 "Int.toText" ati (pure . T)
                            //       (Text.pack . (\x -> if x >= 0 then ("+" <> show x) else show x))
                            // , mk1 "Int.fromText" att (pure . IR.maybeToOptional . fmap I) $
                            //     (\x -> readMaybe (if "+" `List.isPrefixOf` x then drop 1 x else x))
                            //     . Text.unpack
                            // , mk1 "Int.toFloat" ati (pure . F) fromIntegral
                            // , mk1 "Float.abs"           atf (pure . F) abs
                            // , mk1 "Float.toText"        atf (pure . T) (Text.pack . show)
                            // , mk1 "Float.fromText"      att (pure . IR.maybeToOptional . fmap F) (
                            //     (\x -> readMaybe x :: Maybe Double) . Text.unpack)

                            // , mk1 "Text.size" att (pure . N) (fromIntegral . Text.length)
                            // , mk1 "Text.uncons" att
                            //     ( pure
                            //     . IR.maybeToOptional
                            //     . fmap (\(h, t) -> IR.tuple [C h, T t])
                            //     )
                            //     $ Text.uncons
                            // , mk1 "Text.unsnoc" att
                            //     ( pure
                            //     . IR.maybeToOptional
                            //     . fmap (\(i, l) -> IR.tuple [T i, C l])
                            //     )
                            //     $ Text.unsnoc
                            // , mk1 "Text.toCharList" att (pure . Sequence)
                            //     (Sequence.fromList . map C . Text.unpack)

                            // , mk1 "Text.fromCharList" ats (pure . T)
                            //     (\s -> Text.pack [ c | C c <- toList s ])

                            // , mk1 "Char.toNat" atc (pure . N) (fromIntegral . fromEnum)
                            // , mk1 "Char.fromNat" atn (pure . C) (toEnum . fromIntegral)
                            (builtin, two) => Term::PartialNativeApp(builtin.to_owned(), vec![two]),
                        }
                    }
                    Term::Cycle(body, cycle_bindings) => match &*body {
                        Term::ScopedFunction(contents, term, bindings) => match &**contents {
                            ABT::Abs(name, contents) => {
                                info!("Evaling a fn w/ arg {:?}", name.text);
                                let two = two.eval(env, stack);
                                let mut inner_stack = stack.with_frame(term.clone());

                                info!("<stack from cycle bindings>");
                                for (k, v) in &cycle_bindings {
                                    // info!("Recycle {} {:?}", k, v);
                                    inner_stack.push(
                                        k.clone(),
                                        Term::Cycle(Box::new(v.clone()), cycle_bindings.clone()),
                                    );
                                }

                                info!("<stack from scoped bindings>");
                                for (k, v) in bindings {
                                    inner_stack.push(k.clone(), v.clone());
                                }
                                info!("<The vbl>");
                                let fin = inner_stack.with(name.text.clone(), two);
                                info!("Cycle body eval with stack: {:?}", fin.0[0].bindings);

                                let res = contents.eval(env, &fin);
                                info!("<- res {:?}", res);
                                res
                            }
                            contents => unreachable!("Lam {:?}", contents),
                        },
                        _ => unreachable!("cycle child not scoped"),
                    },
                    Term::ScopedFunction(contents, term, bindings) => match &*contents {
                        ABT::Abs(name, contents) => {
                            info!("-> Evaling a fn (arg {})  {:?}", name.text, contents);
                            // info!("-> Bindings")
                            let two = two.eval(env, stack);
                            let mut inner_stack = stack.with_frame(term);
                            info!("<stack from fn bindings>");
                            for (k, v) in bindings {
                                inner_stack.push(k, v);
                            }
                            let fin = inner_stack.with(name.text.clone(), two);
                            info!("Fn body eval with stack: {:?}", fin.0[0].bindings);
                            let res = contents.eval(env, &fin);
                            info!("<- res {:?}", res);
                            res
                        }
                        contents => unreachable!("Lam {:?}", contents),
                    },
                    one => unreachable!("Apply top: {:?}", one),
                }
            }
            _ => unreachable!("Nope {:?}", self),
        }
    }
}
