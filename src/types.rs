use std::cmp::{Eq, PartialEq, PartialOrd};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Symbol {
    pub num: usize,
    pub text: String,
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

#[derive(Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub struct Hash(pub Vec<u8>);

#[derive(Debug, Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub struct Id(pub Hash, pub usize, pub usize);

#[derive(Debug, Clone, std::cmp::Eq, std::cmp::PartialEq, std::hash::Hash, PartialOrd)]
pub enum Referent {
    Ref(Reference),
    Con(Reference, usize, ConstructorType),
}

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

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Term {
    Int(i64),
    Nat(u64),
    Float(f64),
    Boolean(bool),
    Text(String),
    Char(char),
    Blank,
    Ref(Reference),

    PartialNativeApp(String, Vec<Term>),
    PartialConstructor(Reference, usize, Vec<Term>),

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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ABT<Content> {
    Var(Symbol),
    Cycle(Box<ABT<Content>>),
    Abs(Symbol, Box<ABT<Content>>),
    Tm(Content),
}

fn indent(n: usize) -> String {
    let mut res = "".to_owned();
    for _ in 0..n {
        res += "|  ";
    }
    res
}
