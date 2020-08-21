use std::collections::HashMap;

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

#[derive(Clone, Debug)]
pub struct Env {
    // stack: Vec<Frame>,
    root: std::path::PathBuf,
    raw_cache: HashMap<String, ABT<Term>>,
    cache: HashMap<String, Term>,
}

impl Env {
    pub fn init(root: &std::path::Path) -> Self {
        // let root = path.parent().unwrap().parent();
        // let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        Env {
            // stack: vec![],
            root: std::path::PathBuf::from(root),
            raw_cache: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    pub fn load(&mut self, hash: &str) -> ABT<Term> {
        match self.cache.get(hash) {
            Some(v) => ABT::Tm(v.clone()),
            None => match self.raw_cache.get(hash) {
                Some(v) => v.clone(),
                None => {
                    let mut full = self.root.clone();
                    full.push("terms");
                    full.push(hash);
                    full.push("compiled.ub");
                    let res = parser::Buffer::from_file(full.as_path())
                        .unwrap()
                        .get_term();
                    self.raw_cache.insert(hash.to_owned(), res.clone());
                    res
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct Stack(pub Vec<Frame>);

impl Stack {
    fn lookup(&self, name: &str) -> Term {
        for (n, term) in &self.0[0].bindings {
            if n == name {
                return term.clone();
            }
        }
        unreachable!("No term {} found", name);
    }

    fn with(&self, k: String, v: Term) -> Self {
        let mut nw = self.clone();
        nw.0[0].bindings.insert(0, (k, v));
        nw
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    term: String, // TODO Hash
    moment: usize,
    bindings: Vec<(String, Term)>,
}

impl Frame {
    pub fn new(term: String) -> Self {
        Frame {
            term,
            moment: 0,
            bindings: vec![],
        }
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term;
}

impl Eval for ABT<Term> {
    fn eval(&self, env: &mut Env, stack: &Stack) -> Term {
        match self {
            ABT::Var(sym) => stack.lookup(&sym.text),
            ABT::Cycle(inner) => inner.eval(env, stack),
            ABT::Abs(sym, inner) => unreachable!(),
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
            | Term::Blank => self.clone(),
            Term::Let(_, value, contents) => match &**contents {
                ABT::Abs(name, contents) => {
                    let val = value.eval(env, stack);
                    contents.eval(env, &stack.with(name.text.clone(), val))
                }
                _ => unreachable!(),
            },
            Term::Ref(_reference) => unreachable!(),
            Term::App(one, two) => {
                let one = one.eval(env, stack);
                match one {
                    Term::Ref(Reference::Builtin(builtin)) => Term::Int(10),
                    Term::Lam(contents) => match &*contents {
                        ABT::Abs(name, contents) => {
                            let two = two.eval(env, stack);
                            contents.eval(env, &stack.with(name.text.clone(), two))
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
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
