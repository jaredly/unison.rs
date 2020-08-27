use super::env;
use super::types::*;
use std::collections::HashMap;

// So I think we have a scope and a stack?
#[derive(Debug, Clone)]
pub enum IR {
    Handle(usize), // indicate that there's a handler at `usize`
    HandlePure,
    // This means "grab the function identified by usize"
    // but maybe this should be a Term?
    // I mean I should make a different `Value` deal, but not
    // just this moment
    Fn(usize, Vec<(Symbol, usize, usize)>),
    // Builtin(String),
    Cycle(Vec<(Symbol, usize)>),
    // CycleFn(usize, Vec<(Symbol, usize)>),
    // Push this value onto the stack
    Value(Value),
    // lookup the symbol, and push it onto the stack
    PushSym(Symbol, usize),
    // pop the top value off the stack and give it a name
    PopAndName(Symbol, usize),
    // pop the top two values off the stack, call the first with the second
    Call,
    // Swap the top two values
    Swap,
    // Pop the top N values from the stack, assemble into a seq
    Seq(usize),
    JumpTo(usize),
    Mark(usize),
    // pop the last value off the stack;
    // if it's true, advance.
    /// otherwise, jump to the given mark
    If(usize),
    // If2(usize, usize),
    // hmm I might want to short-circut?
    // And,
    // Or,
    // Dup, // duplicate the top item - might not need it
    PopUpOne,
    // Match the given pattern.
    // If the "has_where" flag is true, bound variables
    // will be pushed onto the stack twice
    PatternMatch(Pattern, bool),
    PatternMatchFail,
    MarkStack,
    ClearStackMark,
    // if false, then pop up to the stack mark.
    // if true, the following code will bind those vbls, its fine.
    IfAndPopStack(usize),
}

impl ABT<Term> {
    pub fn to_ir(&self, cmds: &mut IREnv, env: &mut GlobalEnv) {
        match self {
            ABT::Var(symbol, usage) => cmds.push(IR::PushSym(symbol.clone(), *usage)),
            ABT::Tm(term) => term.to_ir(cmds, env),
            ABT::Cycle(inner) => {
                let mut names = vec![];
                let (mut values, body) = unroll_cycle(inner, &mut names);
                for i in 0..names.len() {
                    match values[i].as_mut() {
                        ABT::Tm(Term::Lam(_body, free)) => {
                            // Filter out references to the items in the cycle
                            *free = free
                                .clone()
                                .into_iter()
                                .filter(|x| names.iter().find(|y| x.0 == y.0) == None)
                                .collect();
                        }
                        _ => (),
                    };
                    // match &mut values[i] {
                    //     Term::Lam(body, free) => {

                    //     }
                    // }
                    values[i].to_ir(cmds, env);
                }
                names.reverse();
                cmds.push(IR::Cycle(names));
                body.to_ir(cmds, env);
            }
            ABT::Abs(name, uses, body) => {
                cmds.push(IR::PopAndName(name.clone(), *uses));
                body.to_ir(cmds, env);
            }
        }
    }
}

fn unroll_cycle(
    inner: &ABT<Term>,
    names: &mut Vec<(Symbol, usize)>,
) -> (Vec<Box<ABT<Term>>>, Box<ABT<Term>>) {
    match inner {
        ABT::Abs(sym, uses, inner) => {
            names.push((sym.clone(), *uses));
            match &**inner {
                ABT::Tm(Term::LetRec(_, things, body)) => (things.clone(), body.clone()),
                _ => unroll_cycle(inner, names),
            }
        }
        _ => unreachable!("Cycle not abs"),
    }
}

pub struct GlobalEnv {
    pub env: env::Env,
    pub terms: HashMap<Hash, Vec<IR>>,
    pub types: HashMap<Hash, TypeDecl>,
    pub anon_fns: Vec<(Hash, Vec<IR>)>, // I think?
}

impl GlobalEnv {
    pub fn new(env: env::Env) -> Self {
        GlobalEnv {
            env,
            terms: HashMap::new(),
            types: HashMap::new(),
            anon_fns: vec![],
        }
    }

    pub fn get_type(&mut self, hash: &Hash) -> TypeDecl {
        match self.types.get(hash) {
            Some(v) => v.clone(),
            None => {
                let res = self.env.load_type(&hash.to_string());
                self.types.insert(hash.clone(), res.clone());
                res
            }
        }
    }

    pub fn load(&mut self, hash: &Hash) {
        if self.terms.contains_key(hash) {
            // Already loaded
            return;
        }
        let mut cmds = IREnv::new(hash.clone());
        self.terms.insert(hash.to_owned(), vec![]);
        let term = self.env.load(&hash.to_string());
        // println!("Loaded {}", hash);
        // println!("{:?}", term);
        term.to_ir(&mut cmds, self);

        resolve_marks(&mut cmds.cmds);

        // println!("[how]");
        // for cmd in &cmds.cmds {
        //     println!("{:?}", cmd);
        // }
        // println!("[---]");

        self.terms.insert(hash.to_owned(), cmds.cmds);
    }
    pub fn add_fn(&mut self, hash: Hash, contents: &ABT<Term>) -> usize {
        let mut sub = IREnv::new(hash.clone());
        contents.to_ir(&mut sub, self);

        resolve_marks(&mut sub.cmds);

        let v = self.anon_fns.len();
        self.anon_fns.push((hash, sub.cmds));
        v
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

fn resolve_marks(cmds: &mut Vec<IR>) {
    let marks = make_marks(cmds);
    for cmd in cmds {
        match cmd {
            IR::Handle(mark) => {
                *mark = *marks.get(mark).unwrap();
            }
            IR::JumpTo(mark) => {
                *mark = *marks.get(mark).unwrap();
            }
            IR::IfAndPopStack(mark) => {
                *mark = *marks.get(mark).unwrap();
            }
            IR::If(mark) => {
                *mark = *marks.get(mark).unwrap();
            }
            _ => (),
        }
    }
}

pub struct IREnv {
    pub term: Hash,
    pub cmds: Vec<IR>,
    pub counter: usize,
}

impl IREnv {
    pub fn new(term: Hash) -> Self {
        IREnv {
            term,
            cmds: vec![],
            counter: 0,
        }
    }

    fn push(&mut self, ir: IR) {
        self.cmds.push(ir)
    }

    fn mark(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }
}

impl Term {
    pub fn to_ir(&self, cmds: &mut IREnv, env: &mut GlobalEnv) {
        match self {
            Term::Handle(handler, expr) => {
                /*
                Ok, here's the deal
                the happy path is:

                HandleMark 'Handle -> this would add a handle mark *to the current stack frame*.
                                      they need to travel with the frames.
                expr.to_ir
                WrapAsEffectPure (takes the thing on the stack, wraps it)
                'Handle <-- ok at this point we need to *clear* the 'Handle that's on the frame,
                            because it shouldn't be used twice.
                // here we expect the Request-dealio to be on the stack, right?
                [ handler stuff ]
                // Ok if we're pattern matching on a Request, we can know that we need to re-throw it if it fails.
                'Done

                */

                let handle_mk = cmds.mark();
                cmds.push(IR::Handle(handle_mk));
                expr.to_ir(cmds, env);
                cmds.push(IR::HandlePure);
                cmds.push(IR::Mark(handle_mk));
                handler.to_ir(cmds, env);
                cmds.push(IR::Swap);
                cmds.push(IR::Call);

                let done_mk = cmds.mark();
                cmds.push(IR::Mark(done_mk));
            }
            Term::Ref(Reference::Builtin(_)) => cmds.push(IR::Value(self.clone().into())),
            Term::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                env.load(&hash);
                cmds.push(IR::Value(self.clone().into()))
            }
            Term::App(one, two) => {
                one.to_ir(cmds, env);
                two.to_ir(cmds, env);
                cmds.push(IR::Call)
            }
            Term::Ann(term, _) => term.to_ir(cmds, env),
            Term::Sequence(terms) => {
                let ln = terms.len();
                for inner in terms {
                    inner.to_ir(cmds, env);
                }
                cmds.push(IR::Seq(ln))
            }
            Term::If(cond, yes, no) => {
                let no_tok = cmds.mark();
                let done_tok = cmds.mark();
                cond.to_ir(cmds, env);
                cmds.push(IR::If(no_tok));
                yes.to_ir(cmds, env);
                cmds.push(IR::JumpTo(done_tok));
                cmds.push(IR::Mark(no_tok));
                no.to_ir(cmds, env);
                cmds.push(IR::Mark(done_tok));
            }
            Term::And(a, b) => {
                let fail_tok = cmds.mark();
                let done_tok = cmds.mark();
                a.to_ir(cmds, env);
                cmds.push(IR::If(fail_tok));
                b.to_ir(cmds, env);
                cmds.push(IR::If(fail_tok));
                cmds.push(IR::Value(Value::Boolean(true)));
                cmds.push(IR::JumpTo(done_tok));
                cmds.push(IR::Mark(fail_tok));
                cmds.push(IR::Value(Value::Boolean(false)));
                cmds.push(IR::Mark(done_tok));
            }
            Term::Or(a, b) => {
                let good_tok = cmds.mark();
                let fail_tok = cmds.mark();
                let b_tok = cmds.mark();
                let done_tok = cmds.mark();
                a.to_ir(cmds, env);
                cmds.push(IR::If(b_tok));
                cmds.push(IR::JumpTo(good_tok));
                cmds.push(IR::Mark(b_tok));
                b.to_ir(cmds, env);
                cmds.push(IR::If(fail_tok));

                cmds.push(IR::Mark(good_tok));
                cmds.push(IR::Value(Value::Boolean(true)));
                cmds.push(IR::JumpTo(done_tok));

                cmds.push(IR::Mark(fail_tok));
                cmds.push(IR::Value(Value::Boolean(false)));

                cmds.push(IR::Mark(done_tok));
            }
            Term::Let(_, v, body) => {
                v.to_ir(cmds, env);
                body.to_ir(cmds, env);
            }
            Term::Match(item, arms) => {
                let done_tok = cmds.mark();
                item.to_ir(cmds, env);
                let mut next_tok = cmds.mark();
                for MatchCase(pattern, cond, body) in arms {
                    match cond {
                        None => {
                            cmds.push(IR::PatternMatch(pattern.clone(), false));
                            cmds.push(IR::If(next_tok));
                        }
                        Some(cond) => {
                            // TODO should I have an ID with these,
                            // to catch me of I pop the stack too much?
                            cmds.push(IR::MarkStack);
                            cmds.push(IR::PatternMatch(pattern.clone(), true));
                            cmds.push(IR::IfAndPopStack(next_tok));
                            cond.to_ir(cmds, env);
                            cmds.push(IR::IfAndPopStack(next_tok));
                            cmds.push(IR::ClearStackMark);
                        }
                    }

                    body.to_ir(cmds, env);
                    cmds.push(IR::JumpTo(done_tok));

                    cmds.push(IR::Mark(next_tok));
                    next_tok = cmds.mark();
                }
                cmds.push(IR::PatternMatchFail);
                cmds.push(IR::Mark(done_tok));
                cmds.push(IR::PopUpOne);
            }
            Term::Lam(contents, free_vbls) => {
                let v = env.add_fn(cmds.term.clone(), &**contents);
                cmds.push(IR::Fn(v, free_vbls.clone()));
            }
            Term::Request(Reference::Builtin(name), _) => {
                unimplemented!("Builtin Effect! I dont know the arity: {}", name);
            }
            Term::Request(Reference::DerivedId(id), number) => {
                let t = env.get_type(&id.0);
                match t {
                    TypeDecl::Effect(DataDecl { constructors, .. }) => {
                        let args = calc_args(&constructors[*number].1);
                        // if args == 0 {
                        //     cmds.push(IR::Value(Term::Request(
                        //         Reference::DerivedId(id.clone()),
                        //         *number,
                        //     )))
                        // } else {
                        cmds.push(IR::Value(Value::RequestWithArgs(
                            Reference::DerivedId(id.clone()),
                            *number,
                            args,
                            // ok, this is useless allocation if there are no
                            vec![],
                        )))
                        // }
                    }
                    _ => unimplemented!("ok"),
                }
            }

            _ => cmds.push(IR::Value(self.clone().into())),
        }
    }
}

fn calc_args(t: &ABT<Type>) -> usize {
    match t {
        ABT::Tm(t) => match t {
            Type::Effect(_, _) => 0,
            Type::Arrow(_, inner) => 1 + calc_args(&*inner),
            Type::Forall(inner) => calc_args(inner),
            _ => unimplemented!("Unexpected element of a request {:?}", t),
        },
        ABT::Abs(_, _, inner) => calc_args(inner),
        ABT::Cycle(inner) => calc_args(inner),
        _ => unimplemented!("Unexpected ABT"),
    }
}
