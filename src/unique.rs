use super::types::*;
use std::collections::HashMap;

pub struct Bindings {
    vbls: Vec<Symbol>,
    free: Vec<String>,
    usages: HashMap<Symbol, usize>,
    counter: usize,
}

impl Bindings {
    pub fn new() -> Self {
        Bindings {
            vbls: vec![],
            free: vec![],
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

    fn lookup(&mut self, symbol: &Symbol) -> Symbol {
        match self
            .vbls
            .iter()
            .find(|x| x.text == symbol.text && x.num == symbol.num)
        {
            None => {
                self.free.push(symbol.text.clone());
                symbol.clone()
            }
            Some(s) => s.clone(),
        }
    }

    fn usage(&mut self, symbol: &Symbol) -> usize {
        let n = self.usages.get_mut(symbol).unwrap();
        *n = *n + 1;
        *n
    }
}

impl super::visitor::Visitor for Bindings {
    fn visit_abt(&mut self, abt: &mut ABT<Term>) -> bool {
        match abt {
            ABT::Var(symbol, usage) => {
                *symbol = self.lookup(&symbol);
                *usage = self.usage(&symbol);
            }
            ABT::Abs(sym, _uses, _inner) => {
                self.add(sym);
            }
            _ => (),
        }
        true
    }
    fn post_abt(&mut self, abt: &mut ABT<Term>) {
        match abt {
            ABT::Abs(sym, uses, _inner) => {
                *uses = self.pop(sym);
            }
            _ => (),
        }
    }

    fn visit_term(&mut self, term: &mut Term) -> bool {
        match term {
            Term::Lam(i, free) => {
                let current = self.usages.clone();
                i.accept(self);
                for (k, pre) in current.iter() {
                    let post = self.usages.get(k).unwrap();
                    if post != pre {
                        free.push((k.clone(), *pre, *post));
                    }
                }
                false
            }
            // STOPSHIP here we should reset the usage count for each arm of a Match block, and then take the max ....
            _ => true,
        }
    }
}
