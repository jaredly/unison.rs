use super::types::*;
use std::collections::HashMap;

pub struct Bindings {
    vbls: Vec<Symbol>,
    usages: HashMap<Symbol, usize>,
    counter: usize,
}

impl Bindings {
    pub fn new() -> Self {
        Bindings {
            vbls: vec![],
            usages: HashMap::new(),
            counter: 0,
        }
    }

    fn mark(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }

    fn add(&mut self, symbol: &mut Symbol) {
        let idx = self.mark();
        symbol.unique = idx;
        self.vbls.insert(0, symbol.clone());
        self.usages.insert(symbol.clone(), 0);
    }

    fn pop(&mut self, symbol: &Symbol) -> usize {
        let idx = self.vbls.iter().position(|x| x == symbol).unwrap();
        self.vbls.remove(idx);
        *self.usages.get(symbol).unwrap()
    }

    fn lookup(&self, symbol: &Symbol) -> Symbol {
        self.vbls
            .iter()
            .find(|x| x.text == symbol.text && x.num == symbol.num)
            .unwrap()
            .clone()
    }

    fn usage(&mut self, symbol: &Symbol) -> usize {
        let n = self.usages.get_mut(symbol).unwrap();
        *n = *n + 1;
        *n
    }
}

impl ABT<Term> {
    pub fn unique(&mut self, bindings: &mut Bindings) {
        match self {
            ABT::Var(symbol, usage) => {
                *symbol = bindings.lookup(&symbol);
                *usage = bindings.usage(&symbol);
            }
            ABT::Cycle(inner) => inner.unique(bindings),
            ABT::Abs(sym, uses, inner) => {
                bindings.add(sym); // updates the unique dealio
                inner.unique(bindings);
                *uses = bindings.pop(sym);
            }
            ABT::Tm(inner) => inner.unique(bindings),
        }
    }
}

impl Term {
    fn unique(&mut self, bindings: &mut Bindings) {
        use Term::*;
        match self {
            Handle(a, b) => {
                a.unique(bindings);
                b.unique(bindings)
            }
            App(a, b) => {
                a.unique(bindings);
                b.unique(bindings);
            }
            Ann(a, _) => a.unique(bindings),
            Sequence(a) => {
                for m in a {
                    m.unique(bindings)
                }
            }
            If(a, b, c) => {
                a.unique(bindings);
                b.unique(bindings);
                c.unique(bindings)
            }
            And(a, b) => {
                a.unique(bindings);
                b.unique(bindings)
            }
            Or(a, b) => {
                a.unique(bindings);
                b.unique(bindings)
            }
            Lam(i, free) => {
                let current = bindings.usages.clone();
                i.unique(bindings);
                for (k, pre) in current.iter() {
                    let post = bindings.usages.get(k).unwrap();
                    if post != pre {
                        free.push((k.clone(), *pre, *post));
                    }
                }
            }
            //   -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
            //   -- variables as there are bindings
            LetRec(_, aa, b) => {
                for a in aa {
                    a.unique(bindings);
                }
                b.unique(bindings)
            }
            //   -- Note: first parameter is the binding, second is the expression which may refer
            //   -- to this let bound variable. Constructed as `Let b (abs v e)`
            Let(_, a, b) => {
                a.unique(bindings);
                b.unique(bindings);
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
                a.unique(bindings);
                for m in b {
                    match &mut m.1 {
                        None => (),
                        Some(m) => m.unique(bindings),
                    }
                    m.2.unique(bindings)
                }
            }
            _ => (),
        }
    }
}
