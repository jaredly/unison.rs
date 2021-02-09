use super::env;
use crate::env::Result;
use serde_derive::{Deserialize, Serialize};
use shared::types::*;
use std::collections::HashMap;

fn escape_char(c: &char) -> String {
    let mut res = format!("{}", c.escape_default());
    if !res.starts_with("\\") {
        format!("#\\{}", res)
    } else {
        res = c.escape_unicode().to_string();
        // CHICKEN
        // format!("#\\U+{:0>4}", &res[3..res.len() - 1])
        format!("#\\x{:0>4}", &res[3..res.len() - 1])
    }
}

// Hrmmmm ok so I do need to do some munging of symbols.
// because `w'` isn't valid scheme

fn munge_identifier(id: String) -> String {
    id.replace("'", "-quot")
}

fn atom(s: &str) -> Scheme {
    Scheme::Atom(s.to_owned())
}

fn list(items: Vec<Scheme>) -> Scheme {
    let mut w = 2;
    for item in &items {
        w += item.width();
    }
    Scheme::Apply(items, w)
}

fn vector(items: Vec<Scheme>) -> Scheme {
    let mut w = 3;
    for item in &items {
        w += item.width();
    }
    Scheme::Vector(items, w)
}

fn square(items: Vec<Scheme>) -> Scheme {
    let mut w = 2;
    for item in &items {
        w += item.width();
    }
    Scheme::Square(items, w)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Scheme {
    Atom(String),
    Apply(Vec<Scheme>, usize),
    Square(Vec<Scheme>, usize),
    Vector(Vec<Scheme>, usize),
}

fn white(num: usize) -> String {
    let mut res = String::with_capacity(num);
    for _ in 0..num {
        res.push(' ');
    }
    return res;
}

fn pretty_list(mut left: usize, max_width: usize, items: &Vec<Scheme>) -> (String, usize) {
    if items.len() == 0 {
        return ("".to_owned(), left);
    }
    let mut total = 0;
    for item in items {
        total += item.width();
    }
    if total + left + items.len() - 1 > max_width {
        let (mut res, _first) = items[0].pretty_string(left, max_width);
        if &res == "if" || &res == "let" || &res == "lambda" {
            // left += first;
        }
        // if ["if", "or", "and", "let", "lambda"].contains(&res.as_str()) {
        //     left += first;
        // }
        // let mut idx = 1;
        // if ["if", "lambda"]
        for item in &items[1..] {
            res += "\n";
            res += &white(left);
            let (str, _) = item.pretty_string(left, max_width);
            res += &str;
        }
        return (res, left); // STOPSHIP this left is wrong....
    }
    let (mut res, mut at) = items[0].pretty_string(left, max_width);
    for item in &items[1..] {
        res += " ";
        let (str, att) = item.pretty_string(left, max_width);
        res += &str;
        at += att + 1;
    }
    return (res, at);
}

impl Scheme {
    pub fn width(&self) -> usize {
        use Scheme::*;
        match self {
            Atom(atom) => atom.len(),
            Apply(_items, w) => *w,
            Square(_items, w) => *w,
            Vector(_items, w) => *w,
        }
    }

    pub fn pretty_string(&self, left: usize, max_width: usize) -> (String, usize) {
        use Scheme::*;
        match self {
            Atom(atom) => (atom.to_owned(), left + atom.len()),
            Apply(items, _) => {
                let (mut res, w) = pretty_list(left + 1, max_width, items);
                res.insert(0, '(');
                res.push(')');
                return (res, w + 1);
            }
            Vector(items, _) => {
                let (mut res, w) = pretty_list(left + 2, max_width, items);
                res.insert_str(0, "(vector ");
                res.push(')');
                return (res, w + 1);
            }
            Square(items, _) => {
                let (mut res, w) = pretty_list(left + 1, max_width, items);
                res.insert(0, '[');
                res.push(']');
                return (res, w + 1);
            }
        }
    }

    pub fn to_string(&self) -> String {
        use Scheme::*;
        match self {
            Atom(atom) => atom.clone(),
            Apply(items, _) => {
                "(".to_owned()
                    + &items
                        .iter()
                        .map(|item| item.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                    + ")"
            }
            Vector(items, _) => {
                "(vector ".to_owned()
                    + &items
                        .iter()
                        .map(|item| item.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                    + ")"
            }
            Square(items, _) => {
                "[".to_owned()
                    + &items
                        .iter()
                        .map(|item| item.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                    + "]"
            }
        }
    }
}

pub trait ToChicken {
    fn to_chicken<T: HashLoader>(&self, env: &mut T) -> Result<Scheme>;
}

impl ToChicken for ABT<Term> {
    fn to_chicken<T: HashLoader>(&self, env: &mut T) -> Result<Scheme> {
        match self {
            ABT::Var(symbol, _usage) => Ok(Scheme::Atom(symbol.to_atom())),
            ABT::Tm(term) => term.to_chicken(env),
            ABT::Cycle(inner) => {
                let mut names = vec![];
                let (values, body) = unroll_cycle(inner, &mut names);
                let mut bindings = vec![];
                // let mut buf = String::from("(let (");

                for i in 0..names.len() {
                    bindings.push(list(vec![
                        Scheme::Atom(names[i].0.to_atom()),
                        values[i].to_chicken(env)?,
                    ]));
                }
                return Ok(list(vec![
                    Scheme::Atom("letrec".to_owned()),
                    list(bindings),
                    body.to_chicken(env)?,
                ]));
                // Err(env::Error::NotImplemented("cycle".to_owned()))
                // names.reverse();
                // cmds.push(Scheme::Cycle(names));
                // body.to_chicken(cmds, env)?;
            }
            ABT::Abs(name, _uses, body) => {
                // cmds.push(Scheme::PopAndName(name.clone(), *uses));
                // body.to_chicken(cmds, env)?;
                // return Ok(Scheme::Apply(vec![Scheme::Atom(name.to_atom()), body.to_chicken(env)?]))
                return Ok(list(vec![
                    atom("'bare-abs"),
                    atom(&name.to_atom()),
                    body.to_chicken(env)?,
                ]));
            }
        }
    }
}

fn collect_names(t: &ABT<Type>) -> usize {
    match t {
        ABT::Tm(t) => match t {
            Type::Effect(_, _) => 0,
            Type::Arrow(_, inner) => 1 + collect_names(&*inner),
            Type::Forall(inner) => collect_names(inner),
            _ => unimplemented!("Unexpected element of a request {:?}", t),
        },
        ABT::Abs(_, _, inner) => collect_names(inner),
        ABT::Cycle(inner) => collect_names(inner),
        _ => unimplemented!("Unexpected ABT"),
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

pub struct TranslationEnv {
    pub env: env::Env,
    pub terms: HashMap<Id, (Scheme, ABT<Type>, std::collections::HashMap<Id, bool>)>,
    pub types: HashMap<Id, TypeDecl>,
    pub anon_fns: Vec<(Id, Scheme)>, // I think?
}

// use std::collections::HashSet;

impl TranslationEnv {
    pub fn new(env: env::Env) -> Self {
        TranslationEnv {
            env,
            terms: HashMap::new(),
            types: HashMap::new(),
            anon_fns: vec![],
        }
    }

    pub fn to_string(&self, id: &Id) -> String {
        let mut res = String::from("(define ");
        res.push_str(&id.to_string());
        res.push_str("\n  ");
        let (term, _typ, _) = self.terms.get(id).unwrap();
        res.push_str(&term.pretty_string(2, 100).0);
        res.push_str(")");
        return res;
    }

    pub fn get_type(&mut self, id: &Id) -> TypeDecl {
        match self.types.get(id) {
            Some(v) => v.clone(),
            None => {
                let res = self.env.load_type(&id.to_string());
                self.types.insert(id.clone(), res.clone());
                res
            }
        }
    }

    pub fn load(&mut self, id: &Id) -> Result<HashMap<Id, bool>> {
        if self.terms.contains_key(id) {
            let (_, _, used_hashes) = self.terms.get(id).unwrap();
            // Already loaded
            return Ok(used_hashes.clone());
        }
        // let mut cmds = ChickenEnv::new(hash.clone());
        self.terms.insert(
            id.to_owned(),
            (
                Scheme::Atom("not-yet-evaluated".to_owned()),
                ABT::Tm(Type::Ref(Reference::Builtin("nvm".to_owned()))),
                std::collections::HashMap::new(),
            ),
        );
        let (term, typ) = self.env.load(&id.to_string())?;
        let (res, used_hashes) = {
            let mut loader = Loader {
                stops: 0,
                used_hashes: std::collections::HashMap::new(),
                translation_env: self,
            };
            let res = term.to_chicken(&mut loader);
            (res, loader.used_hashes)
        };
        match res {
            Ok(ch) => self
                .terms
                .insert(id.to_owned(), (ch, typ, used_hashes.clone())),
            Err(env::Error::NotImplemented(text)) => self.terms.insert(
                id.to_owned(),
                (Scheme::Atom(text), typ, used_hashes.clone()),
            ),
            Err(err) => {
                println!("Term broked: {:?}", term);
                return Err(err);
            }
        };
        Ok(used_hashes)
    }

    // pub fn add_fn(&mut self, hash: Id, contents: &ABT<Term>) -> Result<usize> {
    //     // let mut sub = ChickenEnv::new(hash.clone());
    //     let ch = contents.to_chicken(self)?;
    //     // resolve_marks(&mut sub.cmds);
    //     let idx = self.anon_fns.iter().position(|(_, cmds)| *cmds == ch);
    //     Ok(match idx {
    //         None => {
    //             let v = self.anon_fns.len();
    //             self.anon_fns.push((hash, ch));
    //             v
    //         }
    //         Some(idx) => idx,
    //     })
    // }
}

struct Loader<'a> {
    // false = "indirect access (behind a lambda)"
    used_hashes: std::collections::HashMap<Id, bool>,
    translation_env: &'a mut TranslationEnv,
    stops: usize,
}

impl<'a> HashLoader for Loader<'a> {
    fn add(&mut self, hash: &Id) {
        self.used_hashes.insert(hash.clone(), self.stops == 0);
    }
    fn load(&mut self, hash: &Id) -> Result<()> {
        self.add(hash);
        self.translation_env.load(hash)?;
        return Ok(());
    }
    fn get_type(&mut self, hash: &Id) -> TypeDecl {
        self.add(hash);
        return self.translation_env.get_type(hash);
    }
    fn stop_tracking(&mut self) {
        self.stops += 1;
    }
    fn resume_tracking(&mut self) {
        self.stops -= 1;
    }
}

fn ifeq(term: Scheme, cmp: Scheme, yes: Scheme) -> Scheme {
    list(vec![
        atom("if"),
        list(vec![atom("equal?"), term, cmp]),
        yes,
        atom("'fallthrough"),
    ])
}

fn is_effect_pat(pat: &Pattern) -> bool {
    match pat {
        Pattern::EffectBind(_, _, _, _) => true,
        Pattern::EffectPure(_) => true,
        _ => false,
    }
}

// what are we creating?
// an ever-deepening nest, I think
// how do we unwrap that?
// oh do we do it in reverse?
fn pattern_to_chicken<T: HashLoader>(
    env: &mut T,
    pat: &Pattern,
    vbls: &mut Vec<String>,
    term: Scheme,
    mut body: Scheme,
    depth: usize,
) -> Result<Scheme> {
    use Pattern::*;
    Ok(match pat {
        Unbound => body,
        Var => list(vec![
            atom("let"),
            list(vec![list(vec![atom(&vbls.pop().unwrap()), term])]),
            body,
        ]),
        Boolean(b) => ifeq(term, atom(&format!("{}", b)), body),
        Nat(b) => ifeq(term, atom(&format!("{}", b)), body),
        Int(b) => ifeq(term, atom(&format!("{}", b)), body),
        Float(b) => ifeq(term, atom(&format!("{}", b)), body),
        Text(b) => ifeq(term, atom(&format!("{:?}", b)), body),
        Char(b) => ifeq(term, atom(&escape_char(b)), body),
        Constructor(Reference::DerivedId(id), num, innards) => {
            env.add(id);
            if innards.len() == 0 {
                return Ok(list(vec![
                    atom("if"),
                    list(vec![
                        atom("equal?"),
                        atom(&format!("'{}_{}", id.to_string(), num)),
                        term,
                    ]),
                    body,
                    atom("'fallthrough"),
                ]));
            }

            let tmp = atom(&format!("tmp-{}", depth));
            for (i, inner) in innards.iter().enumerate().rev() {
                body = pattern_to_chicken(
                    env,
                    inner,
                    vbls,
                    list(vec![
                        atom("list-ref"),
                        tmp.clone(),
                        atom(&format!("{}", i + 1)),
                    ]),
                    body,
                    depth + 1,
                )?;
            }
            list(vec![
                atom("let"),
                list(vec![list(vec![tmp.clone(), term])]),
                list(vec![
                    atom("if"),
                    list(vec![
                        atom("and"),
                        list(vec![atom("list?"), tmp.clone()]),
                        list(vec![
                            atom("equal?"),
                            atom(&format!("'{}_{}", id.to_string(), num)),
                            list(vec![atom("list-ref"), tmp.clone(), atom("0")]),
                        ]),
                        list(vec![
                            atom("equal?"),
                            list(vec![atom("length"), tmp]),
                            atom(&format!("{}", innards.len() + 1)),
                        ]),
                    ]),
                    body,
                    atom("'fallthrough"),
                ]),
            ])
        }
        // STOPSHIP: make sure to have tests that check the order of bound variables
        SequenceLiteral(items) => {
            let tmp = atom(&format!("tmp-vec-{}", depth));
            for (i, item) in items.iter().enumerate().rev() {
                body = pattern_to_chicken(
                    env,
                    item,
                    vbls,
                    list(vec![
                        atom("vector-ref"),
                        tmp.clone(),
                        atom(&format!("{}", i)),
                    ]),
                    body,
                    depth + 1,
                )?;
            }
            list(vec![
                atom("let"),
                list(vec![list(vec![tmp.clone(), term])]),
                list(vec![
                    atom("if"),
                    list(vec![
                        atom("="),
                        list(vec![atom("vector-length"), tmp]),
                        atom(&format!("{}", items.len())),
                    ]),
                    body,
                    atom("'fallthrough"),
                ]),
            ])
        }
        As(inner) => {
            let name = atom(&vbls.pop().unwrap());
            body = pattern_to_chicken(env, inner, vbls, name.clone(), body, depth + 1)?;
            list(vec![atom("let"), list(vec![list(vec![name, term])]), body])
        }

        EffectPure(inner) => {
            body = pattern_to_chicken(
                env,
                inner,
                vbls,
                list(vec![atom("cadr"), term.clone()]),
                body,
                depth + 1,
            )?;

            list(vec![
                atom("if"),
                list(vec![
                    atom("and"),
                    list(vec![atom("list?"), term.clone()]),
                    list(vec![
                        atom("equal?"),
                        list(vec![atom("length"), term.clone()]),
                        atom("2"),
                    ]),
                    list(vec![
                        atom("equal?"),
                        list(vec![atom("car"), term.clone()]),
                        atom("'pure"),
                    ]),
                ]),
                body,
                atom("'fallthrough"),
            ])
        }

        EffectBind(Reference::DerivedId(id), num, args, kont) => {
            env.add(id);
            body = pattern_to_chicken(
                env,
                kont,
                vbls,
                list(vec![atom("cadr"), term.clone()]),
                body,
                depth + 1,
            )?;
            for (i, arg) in args.iter().enumerate().rev() {
                body = pattern_to_chicken(
                    env,
                    arg,
                    vbls,
                    list(vec![
                        atom("list-ref"),
                        list(vec![
                            atom("caddr"),
                            term.clone(),
                        ]),
                        atom(&format!("{}", i + 1)),
                    ]),
                    body,
                    depth + 1,
                )?;
            }

            list(vec![
                atom("if"),
                list(vec![
                    atom("and"),
                    list(vec![atom("list?"), term.clone()]),
                    list(vec![
                        atom("equal?"),
                        list(vec![atom("length"), term.clone()]),
                        atom(&format!("{}", 4)),
                    ]),
                    list(vec![
                        atom("equal?"),
                        list(vec![atom("car"), term.clone()]),
                        atom("'effect"),
                    ]),
                    list(vec![
                        atom("equal?"),
                        list(vec![atom("length"), list(vec![atom("caddr"), term.clone() ]) ]),
                        atom(&format!("{}", args.len() + 1)),
                    ]),
                    list(vec![
                        atom("equal?"),
                        list(vec![
                            atom("car"),
                            list(vec![atom("caddr"), term.clone()]),
                        ]),
                        atom(&format!("'{}_{}", id.to_string(), num)),
                    ]),
                ]),
                body,
                atom("'fallthrough"),
            ])
        }

        SequenceOp(left, op, right) => {
            let tmp = atom(&format!("snoc-tmp-{}", depth));

            let (left_val, right_val, min_size) = match op {
                SeqOp::Snoc => {
                    let len_minus_1 = list(vec![
                        atom("-"),
                        list(vec![atom("vector-length"), tmp.clone()]),
                        atom("1"),
                    ]);
                    (
                        list(vec![
                            list(vec![atom("List.take"), len_minus_1.clone()]),
                            tmp.clone(),
                        ]),
                        list(vec![atom("vector-ref"), tmp.clone(), len_minus_1]),
                        1,
                    )
                }
                SeqOp::Cons => (
                    list(vec![atom("vector-ref"), tmp.clone(), atom("0")]),
                    list(vec![list(vec![atom("List.drop"), atom("1")]), tmp.clone()]),
                    1,
                ),
                SeqOp::Concat => {
                    let (count, take) = match (&**left, &**right) {
                        (SequenceLiteral(items), _) => {
                            (items.len(), atom(&format!("{}", items.len())))
                        }
                        (_, SequenceLiteral(items)) => (
                            items.len(),
                            list(vec![
                                atom("-"),
                                list(vec![atom("vector-length"), tmp.clone()]),
                                atom(&format!("{}", items.len())),
                            ]),
                        ),
                        _ => unreachable!("concat must have a sequence literal on one side"),
                    };

                    (
                        list(vec![
                            list(vec![atom("List.take"), take.clone()]),
                            tmp.clone(),
                        ]),
                        list(vec![list(vec![atom("List.drop"), take]), tmp.clone()]), // TODO
                        count,
                    )
                }
            };

            body = pattern_to_chicken(env, right, vbls, right_val, body, depth + 1)?;
            body = pattern_to_chicken(env, left, vbls, left_val, body, depth + 1)?;
            list(vec![
                atom("let"),
                list(vec![list(vec![tmp.clone(), term])]),
                list(vec![
                    atom("if"),
                    list(vec![
                        atom(">="),
                        list(vec![atom("vector-length"), tmp]),
                        atom(&format!("{}", min_size)),
                        // atom("0"),
                    ]),
                    body,
                    atom("'fallthrough"),
                ]),
            ])
        }
        _ => atom(&format!("{:?}", format!("{:?}", pat))),
    })
}

pub trait HashLoader {
    fn load(&mut self, hash: &Id) -> Result<()>;
    fn add(&mut self, hash: &Id);
    fn get_type(&mut self, hash: &Id) -> TypeDecl;
    fn stop_tracking(&mut self);
    fn resume_tracking(&mut self);
    // fn notrack<T: HashLoader>(&mut self) -> &mut T;
}

// impl HashLoader for TranslationEnv {
//     fn notrack(&mut self) -> &mut TranslationEnv {
//         return self;
//     }
//     fn add(&mut self, _hash: &Id) {
//         // ignored
//     }
//     fn load(&mut self, hash: &Id) -> Result<()> {
//         let _ = self.load(hash);
//         Ok(())
//     }
//     fn get_type(&mut self, hash: &Id) -> TypeDecl {
//         self.get_type(hash)
//     }
// }

impl ToChicken for Term {
    fn to_chicken<T: HashLoader>(&self, env: &mut T) -> Result<Scheme> {
        match self {
            Term::Request(Reference::DerivedId(id), number) => {
                let args_count = match env.get_type(&id) {
                    TypeDecl::Effect(DataDecl { constructors, .. }) => {
                        calc_args(&constructors[*number].1)
                    }
                    _ => unreachable!("effect type not found"),
                };

                if args_count == 0 {
                    Ok(list(vec![atom(&format!("{}_{}", id.to_string(), number))]))
                } else {
                    Ok(atom(&format!("{}_{}", id.to_string(), number)))
                }
            }
            Term::Handle(handler, expr) => {
                env.stop_tracking();
                let ex = expr.to_chicken(env);
                env.resume_tracking();
                Ok(list(vec![
                    atom("handle-ability"),
                    list(vec![atom("lambda"), atom("()"), ex?]),
                    handler.to_chicken(env)?,
                ]))
                // Err(env::Error::NotImplemented("handle".to_owned()))
            }
            Term::Ref(Reference::Builtin(name)) => Ok(Scheme::Atom(name.clone())),
            Term::Ref(Reference::DerivedId(id)) => {
                env.load(&id)?;
                Ok(Scheme::Atom(id.to_string()))
            }
            Term::App(one, two) => Ok(list(vec![one.to_chicken(env)?, two.to_chicken(env)?])),
            Term::Int(num) => Ok(Scheme::Atom(num.to_string())),
            Term::Float(num) => Ok(Scheme::Atom(num.to_string())),
            Term::Nat(num) => Ok(Scheme::Atom(num.to_string())),
            Term::Boolean(num) => Ok(Scheme::Atom(num.to_string())),
            Term::Text(num) => Ok(Scheme::Atom(format!("{:?}", num))),
            Term::Char(num) => Ok(Scheme::Atom(escape_char(num))),
            Term::If(cond, yes, no) => Ok(list(vec![
                Scheme::Atom("if".to_owned()),
                cond.to_chicken(env)?,
                yes.to_chicken(env)?,
                no.to_chicken(env)?,
            ])),
            Term::And(one, two) => Ok(list(vec![
                Scheme::Atom("and".to_owned()),
                one.to_chicken(env)?,
                two.to_chicken(env)?,
            ])),
            Term::Or(one, two) => Ok(list(vec![
                Scheme::Atom("or".to_owned()),
                one.to_chicken(env)?,
                two.to_chicken(env)?,
            ])),
            // A constructor is a function that takes a number of
            // arguments, and returns a list with its name as the first item.
            Term::Constructor(Reference::Builtin(name), num) => {
                Ok(Scheme::Atom(format!("{}_{}", name, num)))
            }
            Term::Constructor(Reference::DerivedId(id), num) => {
                env.get_type(id);
                Ok(Scheme::Atom(format!("{}_{}", id.to_string(), num)))
            }
            Term::Sequence(items) => {
                let mut res = vec![];
                for item in items {
                    res.push(item.to_chicken(env)?);
                }
                return Ok(vector(res));
            }
            // Term::LetRec(_, bound, body) => match &**body {
            //     ABT::Abs(name, _, body) => Ok(Scheme::Apply(vec![
            //         atom("letrec"),
            //         list(vec![
            //             list(vec![
            //                 atom(&name.to_atom()),
            //                 bound.to_chicken(env)?,
            //             ])
            //         ]),
            //         body.to_chicken(env)?,
            //     ])),
            //     _ => unimplemented!(),
            // },
            Term::Let(_, bound, body) => match &**body {
                ABT::Abs(name, _, body) => Ok(list(vec![
                    atom("let"),
                    list(vec![list(vec![
                        atom(&name.to_atom()),
                        bound.to_chicken(env)?,
                    ])]),
                    body.to_chicken(env)?,
                ])),
                _ => unimplemented!(),
            },
            Term::Lam(contents, _) => {
                env.stop_tracking();
                let res = match &**contents {
                    ABT::Abs(name, _, body) => Ok(list(vec![
                        atom("lambda"),
                        list(vec![atom(&name.to_atom())]),
                        body.to_chicken(env)?,
                    ])),
                    _ => unimplemented!(),
                };
                env.resume_tracking();
                return res;
                // Ok(atom("lambda I think"))
            }
            Term::Match(value, cases) => {
                let mut result = list(vec![atom("no-match")]);
                let tmp = atom("tmp-match-head");
                result = if cases
                    .iter()
                    .any(|MatchCase(pattern, _, _)| is_effect_pat(pattern))
                {
                    list(vec![atom("rethrow-effect"), tmp.clone()])
                } else {
                    result
                };
                // STOPSHIP: add a `_ (rethrow-effect)` if this is an effect matcher
                for MatchCase(pattern, cond, body) in cases.iter().rev() {
                    let (mut vbls, body) = get_vbls(body);
                    vbls.reverse();
                    let mut vblnames = vbls
                        .iter()
                        .map(|name| atom(&format!("'{}", name)))
                        .collect::<Vec<Scheme>>();
                    vblnames.insert(0, atom("list"));
                    let mut body = match cond {
                        None => body.to_chicken(env)?,
                        Some(cond) => {
                            list(vec![
                                atom("if"),
                                // do I just strip args?
                                strip_args(cond).to_chicken(env)?,
                                body.to_chicken(env)?,
                                atom("'fallthrough"),
                            ])
                        }
                    };
                    body = pattern_to_chicken(env, &pattern, &mut vbls, tmp.clone(), body, 0)?;
                    result = list(vec![
                        atom("let"),
                        list(vec![
                            // list(vec![atom("_vblnames"), list(vblnames)]),
                            list(vec![atom("result"), body]),
                        ]),
                        list(vec![
                            atom("if"),
                            list(vec![atom("equal?"), atom("'fallthrough"), atom("result")]),
                            result,
                            atom("result"),
                        ]),
                    ]);
                }
                Ok(list(vec![
                    atom("let"),
                    list(vec![list(vec![tmp, value.to_chicken(env)?])]),
                    result,
                ]))
            }
            Term::Ann(inner, _) => inner.to_chicken(env),
            Term::Blank => unreachable!("blank found"),
            Term::TermLink(referent) => Ok(list(vec![
                atom("term-link"),
                atom(&format!("'{}", referent.to_atom())),
            ])),
            Term::TypeLink(reference) => Ok(list(vec![
                atom("type-link"),
                atom(&format!("'{}", reference.to_atom())),
            ])),
            _ => Ok(Scheme::Atom(format!(
                "(not-implemented {:?})",
                format!("{:?}", self)
            ))),
        }
    }
}

fn strip_args(body: &ABT<Term>) -> &ABT<Term> {
    match body {
        ABT::Abs(_, _, inner) => strip_args(inner),
        _ => body,
    }
}

fn get_vbls(body: &ABT<Term>) -> (Vec<String>, &ABT<Term>) {
    match body {
        ABT::Abs(name, _, inner) => {
            let (mut vbls, body) = get_vbls(inner);
            vbls.push(name.to_atom());
            return (vbls, body);
        }
        _ => (vec![], body),
    }
}

pub fn ability_to_chicken(name: &str, t: &ABT<Type>) -> Scheme {
    let args = calc_args(t);
    let mut vbls = vec![];
    for i in 0..args {
        vbls.push(atom(&format!("arg_{}", i)));
    }
    let mut body = list(vec![
        atom("call/cc"),
        list(vec![
            atom("lambda"),
            list(vec![atom("k")]),
            list(vec![
                atom("throw-effect"),
                atom("k"),
                list(
                    vec![atom("list"), atom(&format!("'{}", name))]
                        .into_iter()
                        .chain(vbls.iter().cloned())
                        .collect(),
                ),
            ]),
        ]),
    ]);
    for vbl in vbls.iter().rev() {
        body = list(vec![atom("lambda"), list(vec![vbl.clone()]), body]);
    }
    if args == 0 {
        body = list(vec![atom("lambda"), list(vec![]), body]);
    }
    return list(vec![atom("define"), atom(name), body]);
}

// lol scheme -> type
pub fn ability_to_type(name: &str, t: &ABT<Type>) -> Scheme {
    let args = calc_args(t);
    if args == 0 {
        return list(vec![
            atom("define"),
            atom(name),
            atom(&format!("'{}", name)),
        ]);
    }
    let mut vbls = vec![];
    for i in 0..args {
        vbls.push(atom(&format!("arg_{}", i)));
    }
    let mut body = list(
        vec![atom("list"), atom(&format!("'{}", name))]
            .into_iter()
            .chain(vbls.iter().cloned())
            .collect(),
    );
    for vbl in vbls.iter().rev() {
        body = list(vec![atom("lambda"), list(vec![vbl.clone()]), body]);
    }
    // if args == 0 {
    //     body = list(vec![atom("lambda"), list(vec![]), body]);
    // }
    return list(vec![atom("define"), atom(name), body]);
}

fn calc_args(t: &ABT<Type>) -> usize {
    match t {
        ABT::Tm(t) => match t {
            Type::Effect(_, _) => 0,
            Type::Arrow(_, inner) => 1 + calc_args(&*inner),
            Type::Forall(inner) => calc_args(inner),
            // Type::Ref(_) => 0,
            // _ => unimplemented!("Unexpected element of a request {:?}", t),
            _ => 0,
        },
        ABT::Abs(_, _, inner) => calc_args(inner),
        ABT::Cycle(inner) => calc_args(inner),
        _ => unimplemented!("Unexpected ABT"),
    }
}
