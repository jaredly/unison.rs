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
            Term::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                env.load(&hash.to_string()).eval(env, stack)
            }
            Term::Ref(Reference::Builtin(contents)) => {
                match contents.as_str() {
                    "Text.empty" => Term::Text("".to_string()),
                    "Sequence.empty" => Term::Sequence(vec![]),
                    // "Bytes.empty" => Term::BYtes(""),
                    _ => self.clone(),
                }
            },

            Term::Constructor(_, _) => self.clone(),
            Term::Request(_, _) => unimplemented!("Request {:?}", self),
            Term::Handle(contents, handler) => unimplemented!("Handle {:?}", self),
            Term::LetRec(_, values, body) => unimplemented!("LetRec {:?}", self),
            Term::Match(arms, body) => unimplemented!("Match {:?}", self),

            Term::Ann(term, _type) => term.eval(env, stack),
            Term::Sequence(contents) => {
                let mut res = vec![];
                for item in contents.iter() {
                    res.push(Box::new(ABT::Tm(item.eval(env, stack))))
                };
                Term::Sequence(res)
            },
            Term::If(one, two, three) => match one.eval(env, stack) {
                Term::Boolean(true) => two.eval(env, stack),
                Term::Boolean(false) => three.eval(env, stack),
                _ => unreachable!("If with not a bool {:?}", self),
            }
            Term::And(one, two) => match (one.eval(env, stack), two.eval(env, stack)) {
                (Term::Boolean(a), Term::Boolean(b)) => Term::Boolean(a && b),
                (a, b) => unreachable!("and not bool {:?} and {:?}", a, b)
            }
            Term::Or(one, two) => match (one.eval(env, stack), two.eval(env, stack)) {
                (Term::Boolean(a), Term::Boolean(b)) => Term::Boolean(a || b),
                (a, b) => unreachable!("or not bool {:?} and {:?}", a, b)
            }
            Term::Lam(contents) => Term::Lam(Box::new(ABT::Tm(contents.eval(env, stack)))),
            // Term::LetRec(
            
            Term::App(one, two) => {
                let one = one.eval(env, stack);
                match one {
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
                            ("Int.pow", [Term::Int(a)], Term::Nat(b)) => Term::Int(a.pow(*b as u32)),
                            ("Int.shiftLeft", [Term::Int(a)], Term::Nat(b)) => Term::Int(a >> *b as u32),
                            ("Int.shiftRight", [Term::Int(a)], Term::Nat(b)) => Term::Int(a << *b as u32),

                            ("Nat.+", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a + b),
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
                            ("Nat.pow", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a.pow(*b as u32)),
                            ("Nat.shiftLeft", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a >> *b as u32),
                            ("Nat.shiftRight", [Term::Nat(a)], Term::Nat(b)) => Term::Nat(a << *b as u32),

                            // , ("Nat.drop", 2, DropN (Slot 1) (Slot 0))
                            // , ("Nat.sub", 2, SubN (Slot 1) (Slot 0))
                            // , ("Nat.mod", 2, ModN (Slot 1) (Slot 0))
                            // , ("Nat.pow", 2, PowN (Slot 1) (Slot 0))

                            ("Float.+", [Term::Float(a)], Term::Float(b)) => Term::Float(a + b),
                            ("Float.-", [Term::Float(a)], Term::Float(b)) => Term::Float(a - b),
                            ("Float.*", [Term::Float(a)], Term::Float(b)) => Term::Float(a * b),
                            ("Float./", [Term::Float(a)], Term::Float(b)) => Term::Float(a / b),
                            ("Float.<", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a < *b),
                            ("Float.<=", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a <= *b),
                            ("Float.>", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a > *b),
                            ("Float.>=", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a >= *b),
                            ("Float.==", [Term::Float(a)], Term::Float(b)) => Term::Boolean(*a == *b),

                            ("Universal.==", [one], two) => Term::Boolean(one == two),
                            ("Universal.>", [one], two) => Term::Boolean(one > two),
                            ("Universal.<", [one], two) => Term::Boolean(one < two),
                            ("Universal.>=", [one], two) => Term::Boolean(one >= two),
                            ("Universal.<=", [one], two) => Term::Boolean(one <= two),
                            ("Universal.compare", [one], two) => Term::Int(
                                if one < two {
                                    -1
                                } else if one > two {
                                    1
                                } else {
                                    0
                                }
                            ),
                            // , ("Universal.compare", 2, CompareU (Slot 1) (Slot 0))

                            (a, b, c) => unreachable!("Native app, we dont have more than two args {} - {:?} - {:?}", a, b, c),
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

                            // , ("Int.complement", 1, ComplementI (Slot 0))
                            // , ("Int.signum", 1, SignumI (Slot 0))
                            // , ("Int.truncate0", 1, Truncate0I (Slot 0))
                            // , ("Int.leadingZeros", 1, LeadZeroI (Slot 0))
                            // , ("Int.trailingZeros", 1, TrailZeroI (Slot 0))
                            // , ("bug", 1, Bug (Slot 0))
                            // , ("todo", 1, Todo (Slot 0))
                            // , ("Nat.complement", 1, ComplementN (Slot 0))
                            // , ("Nat.leadingZeros", 1, LeadZeroN (Slot 0))
                            // , ("Nat.trailingZeros", 1, TrailZeroN (Slot 0))
                            (builtin, two) => Term::PartialNativeApp(builtin.to_owned(), vec![two])
                        }
                    }
                    Term::Lam(contents) => match &*contents {
                        ABT::Abs(name, contents) => {
                            let two = two.eval(env, stack);
                            contents.eval(env, &stack.with(name.text.clone(), two))
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
