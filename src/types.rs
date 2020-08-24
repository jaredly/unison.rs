use std::cmp::{PartialEq, PartialOrd};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Symbol {
    pub num: usize,
    pub text: String,
}
impl Symbol {
    pub fn new(text: String) -> Self {
        Symbol { text, num: 0 }
    }
}

#[derive(Debug, Clone, Copy, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub enum ConstructorType {
    Data,
    Effect,
}

#[derive(Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub enum Reference {
    Builtin(String),
    DerivedId(Id),
}

impl Reference {
    pub fn from_hash(hash: &str) -> Self {
        Reference::DerivedId(Id(Hash::from_string(hash), 0, 1))
    }
}

#[derive(Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub struct Hash(pub Vec<u8>);

#[derive(Debug, Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub struct Id(pub Hash, pub usize, pub usize);

#[derive(Debug, Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub enum Referent {
    Ref(Reference),
    Con(Reference, usize, ConstructorType),
}

// impl Referent {
//     fn reference(&self) -> &Reference {
//         match self {
//             Referent::Ref(r) => r,
//             Referent::Con(r, _, _) => r,
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct MatchCase(pub Pattern, pub Option<Box<ABT<Term>>>, pub Box<ABT<Term>>);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Pattern {
    Unbound,
    Var,
    Boolean(bool),
    Int(i64),
    Nat(u64),
    Float(f64),
    Text(String),
    Char(char),
    Constructor(Reference, usize, Vec<Pattern>),
    As(Box<Pattern>),
    EffectPure(Box<Pattern>),
    EffectBind(Reference, usize, Vec<Pattern>, Box<Pattern>),
    SequenceLiteral(Vec<Pattern>),
    SequenceOp(Box<Pattern>, SeqOp, Box<Pattern>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SeqOp {
    Cons,
    Snoc,
    Concat,
}

// Base functor for types in the Unison language
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    Ref(Reference),
    Arrow(Box<ABT<Type>>, Box<ABT<Type>>),
    Ann(Box<ABT<Type>>, Kind),
    App(Box<ABT<Type>>, Box<ABT<Type>>),
    Effect(Box<ABT<Type>>, Box<ABT<Type>>),
    Effects(Vec<ABT<Type>>),
    Forall(Box<ABT<Type>>),
    //  binder like ∀, used to introduce variables that are
    //  bound by outer type signatures, to support scoped type
    //  variables
    IntroOuter(Box<ABT<Type>>),
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, PartialOrd)]
pub enum Term {
    Int(i64),
    Nat(u64),
    Float(f64),
    Boolean(bool),
    Text(String),
    Bytes(Vec<u64>),
    Char(char),
    Blank,
    Ref(Reference),

    CycleFnBody(usize, Vec<(Symbol, Term)>, Vec<(Symbol, usize)>),
    PartialFnBody(usize, Vec<(Symbol, Term)>),
    PartialNativeApp(String, Vec<Term>),
    PartialConstructor(Reference, usize, Vec<Term>),
    ScopedFunction(Box<ABT<Term>>, String, Vec<(String, Term)>),
    // CycleFunction(Box<ABT<Term>>, String, Vec<(String, Term)>, String),
    Cycle(Box<Term>, Vec<(String, Term)>),

    Constructor(Reference, usize),
    Request(Reference, usize),
    Handle(Box<ABT<Term>>, Box<ABT<Term>>),
    App(Box<ABT<Term>>, Box<ABT<Term>>),
    Ann(Box<ABT<Term>>, ABT<Type>),
    Sequence(Vec<Box<ABT<Term>>>),
    If(Box<ABT<Term>>, Box<ABT<Term>>, Box<ABT<Term>>),
    And(Box<ABT<Term>>, Box<ABT<Term>>),
    Or(Box<ABT<Term>>, Box<ABT<Term>>),
    Lam(Box<ABT<Term>>),
    //   -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    //   -- variables as there are bindings
    LetRec(bool, Vec<Box<ABT<Term>>>, Box<ABT<Term>>),
    //   -- Note: first parameter is the binding, second is the expression which may refer
    //   -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let(bool, Box<ABT<Term>>, Box<ABT<Term>>),
    //   -- Pattern matching / eliminating data types, example:
    //   --  case x of
    //   --    Just n -> rhs1
    //   --    Nothing -> rhs2
    //   --
    //   -- translates to
    //   --
    //   --   Match x
    //   --     [ (Constructor 0 [Var], ABT.abs n rhs1)
    //   --     , (Constructor 1 [], rhs2) ]
    Match(Box<ABT<Term>>, Vec<MatchCase>),
    TermLink(Referent),
    TypeLink(Reference),
}

// impl Term {
//     fn walk<F>(&self, f: F) where F: Fn(&Term) -> () {
//         f(self);
//         match self {
//             Match(a, b) =>
//             Handle(Box<ABT<Term>>, Box<ABT<Term>>),
//             App(Box<ABT<Term>>, Box<ABT<Term>>),
//             Ann(Box<ABT<Term>>, ABT<Type>),
//             Sequence(Vec<Box<ABT<Term>>>),
//             If(Box<ABT<Term>>, Box<ABT<Term>>, Box<ABT<Term>>),
//             And(Box<ABT<Term>>, Box<ABT<Term>>),
//             Or(Box<ABT<Term>>, Box<ABT<Term>>),
//             Lam(Box<ABT<Term>>),
//             //   -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
//             //   -- variables as there are bindings
//             LetRec(bool, a, b) => {
//                 for m in a {
//                     m.walk(f);
//                 }
//                 b.walk(f);
//             }
//             //   -- Note: first parameter is the binding, second is the expression which may refer
//             //   -- to this let bound variable. Constructed as `Let b (abs v e)`
//             Let(bool, a, b) => {
//                 a.walk(f); b.walk(f);
//             }
//             ScopedFunction(a, _, _) => a.walk(f)
//         }
//     }
// }

#[derive(Clone, PartialEq, PartialOrd)]
pub enum ABT<Content> {
    Var(Symbol),
    Cycle(Box<ABT<Content>>),
    Abs(Symbol, Box<ABT<Content>>),
    Tm(Content),
}

// fn indent(n: usize) -> String {
//     let mut res = "".to_owned();
//     for _ in 0..n {
//         res += "|  ";
//     }
//     res
// }

#[derive(Debug, Clone, std::cmp::Eq, std::hash::Hash, std::cmp::PartialEq)]
pub struct NameSegment {
    pub text: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Modifier {
    Structural,
    Unique(String),
    Opaque,
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    Effect(DataDecl),
    Data(DataDecl),
}

#[derive(Debug, Clone)]
pub struct DataDecl {
    pub modifier: Modifier,
    pub bound: Vec<Symbol>,
    pub constructors: Vec<(Symbol, ABT<Type>)>,
}

#[derive(Debug, Clone)]
pub enum Causal<Contents> {
    One(Contents),
    Cons(Hash, Contents),
    Merge(Vec<Hash>, Contents),
}

#[derive(Debug, Clone)]
pub struct RawBranch {
    pub terms: Star<Referent, NameSegment>,
    pub types: Star<Reference, NameSegment>,
    pub children: HashMap<NameSegment, Hash>,
    pub edits: HashMap<NameSegment, Hash>,
}

impl RawBranch {
    pub fn merge(&mut self, other: &RawBranch) {
        self.terms.merge(&other.terms);
        self.types.merge(&other.types);
        self.children.extend(other.children.clone());
        self.edits.extend(other.edits.clone());
    }
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub raw: RawBranch,
    pub children: HashMap<NameSegment, Branch>,
}

#[derive(Debug, Clone)]
pub struct Star<K, V> {
    pub fact: std::collections::HashSet<K>,
    pub d1: HashMap<K, V>,
    pub d2: HashMap<K, Reference>,
    pub d3: HashMap<K, (Reference, Reference)>,
}

impl<K: std::hash::Hash + std::cmp::Eq + Clone, V: Clone> Star<K, V> {
    fn merge(&mut self, other: &Self) {
        self.fact.extend(other.fact.clone());
        self.d1.extend(other.d1.clone());
        self.d2.extend(other.d2.clone());
        self.d3.extend(other.d3.clone());
    }
}
