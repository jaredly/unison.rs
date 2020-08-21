use std::collections::HashMap;

use super::env::*;
use super::parser;
use super::types::*;

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

impl ABT<Term> {
    pub fn eval_with_bindings(&self, env: &mut Env, stack: &Stack, bindings: Vec<Term>) -> Term {
        // let kvs = vec![];
        let mut body = self;
        let mut new_stack = stack.clone();
        for binding in bindings.into_iter() {
            match body {
                ABT::Abs(sym, inner) => {
                    new_stack.set(sym.text.clone(), binding);
                    body = inner;
                }
                _ => unreachable!("NOP"),
            }
        }
        return body.eval(env, &new_stack);
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

impl Eval for ABT<Term> {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term {
        match self {
            ABT::Var(sym) => stack.lookup(&sym.text),
            ABT::Cycle(inner) => {
                let mut names = vec![];
                let (values, body) = unroll_cycle(inner, &mut names);
                let mut new_stack = stack.clone();
                for i in 0..names.len() {
                    new_stack.set(
                        names[i].clone(),
                        values[values.len() - 1 - i].eval(env, stack),
                    );
                }
                body.eval(env, stack)
            }
            ABT::Abs(sym, inner) => unreachable!("Raw abs {}", stack.0[0].term),
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
            | Term::Blank => self.clone(),
            Term::Let(_, value, contents) => match &**contents {
                ABT::Abs(name, contents) => {
                    let val = value.eval(env, stack);
                    contents.eval(env, &stack.with(name.text.clone(), val))
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
            Term::Handle(contents, handler) => unimplemented!("Handle {:?}", self),
            Term::LetRec(_, values, body) => unimplemented!("LetRec {:?}", self),
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
                unreachable!("Nothing matched {:?}\n{:?}", term, arms);
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
                            // , ("Universal.compare", 2, CompareU (Slot 1) (Slot 0))
                            (a, b, c) => unreachable!(
                                "Native app, we dont have more than two args {} - {:?} - {:?}",
                                a, b, c
                            ),
                        }
                    }

                    Term::Ref(Reference::Builtin(builtin)) => {
                        match (builtin.as_str(), two.eval(env, stack)) {
                            (builtin, two) => Term::PartialNativeApp(builtin.to_owned(), vec![two]),
                        }
                    }
                    Term::ScopedFunction(contents, term, bindings) => match &*contents {
                        ABT::Abs(name, contents) => {
                            let two = two.eval(env, stack);
                            let mut inner_stack = stack.with_frame(term);
                            for (k, v) in bindings {
                                inner_stack.set(k, v);
                            }
                            contents.eval(env, &inner_stack.with(name.text.clone(), two))
                        }
                        contents => unreachable!("Lam {:?}", contents),
                    },
                    one => unreachable!("Apply top: {:?}", one),
                }
            }
            _ => unreachable!(),
        }
    }
}

// What things can be moments?
// Just function applications, I think.
// Nothing else can be a moment.
// So as we traverse a term, we count the function applications we encounter.

// So the goodish way to do this would be with an immutable data structure, but we're just gonna clone out the wazoo
