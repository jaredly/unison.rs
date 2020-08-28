use super::types::*;

pub trait Visitor {
    fn visit_abt(&mut self, abt: &mut ABT<Term>) -> bool;
    fn visit_term(&mut self, term: &mut Term) -> bool;
    fn post_abt(&mut self, abt: &mut ABT<Term>);
}

impl ABT<Term> {
    pub fn accept<T: Visitor>(&mut self, visitor: &mut T) {
        if !visitor.visit_abt(self) {
            return;
        }
        match self {
            ABT::Cycle(inner) => inner.accept(visitor),
            ABT::Abs(sym, uses, inner) => {
                inner.accept(visitor);
            }
            ABT::Tm(inner) => inner.accept(visitor),
            _ => (),
        }
        visitor.post_abt(self);
    }
}

impl Term {
    fn accept<T: Visitor>(&mut self, visitor: &mut T) {
        if !visitor.visit_term(self) {
            return;
        }
        use Term::*;
        match self {
            Handle(a, b) => {
                a.accept(visitor);
                b.accept(visitor)
            }
            App(a, b) => {
                a.accept(visitor);
                b.accept(visitor);
            }
            Ann(a, _) => a.accept(visitor),
            Sequence(a) => {
                for m in a {
                    m.accept(visitor)
                }
            }
            If(a, b, c) => {
                a.accept(visitor);
                b.accept(visitor);
                c.accept(visitor)
            }
            And(a, b) => {
                a.accept(visitor);
                b.accept(visitor)
            }
            Or(a, b) => {
                a.accept(visitor);
                b.accept(visitor)
            }
            Lam(i, free) => {
                i.accept(visitor);
            }
            //   -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
            //   -- variables as there are bindings
            LetRec(_, aa, b) => {
                for a in aa {
                    a.accept(visitor);
                }
                b.accept(visitor)
            }
            //   -- Note: first parameter is the binding, second is the expression which may refer
            //   -- to this let bound variable. Constructed as `Let b (abs v e)`
            Let(_, a, b) => {
                a.accept(visitor);
                b.accept(visitor);
            }
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
            Match(a, b) => {
                // STOPSHIP here we should reset the usage count for each arm, and then take the max ....
                a.accept(visitor);
                for m in b {
                    match &mut m.1 {
                        None => (),
                        Some(m) => m.accept(visitor),
                    }
                    m.2.accept(visitor)
                }
            }
            _ => (),
        }
    }
}
