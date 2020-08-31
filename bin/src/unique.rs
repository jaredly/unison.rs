use super::visitor::Visitor;
use crate::visitor::Accept;
use log::info;
use shared::types::*;
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
        let at = self.counter;
        self.counter += 1;
        at
    }

    // fn add_owned(&mut self, mut symbol: Symbol) {
    //     let idx = self.mark();
    //     symbol.unique = idx;
    //     self.vbls.insert(0, symbol.clone());
    //     self.usages.insert(symbol, 0);
    // }

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
        info!("Lookup {:?}", symbol);
        self.vbls.iter().find(|x| x.pre_eq(symbol)).unwrap().clone()
    }

    fn usage(&mut self, symbol: &Symbol) -> usize {
        let n = self.usages.get_mut(symbol).unwrap();
        *n = *n + 1;
        *n
    }
}

pub struct FreeFinder {
    bound: Vec<Symbol>,
    free: Vec<Symbol>,
}

impl FreeFinder {
    fn new() -> Self {
        FreeFinder {
            bound: vec![],
            free: vec![],
        }
    }
    fn lookup(&mut self, sym: &Symbol) {
        if self.bound.iter().find(|x| x.pre_eq(&sym)) == None
            && self.free.iter().find(|x| x.pre_eq(&sym)) == None
        {
            self.free.push(sym.clone());
        }
    }
    fn add(&mut self, sym: &Symbol) {
        self.bound.insert(0, sym.clone());
    }
    fn rm(&mut self, sym: &Symbol) {
        let idx = self.bound.iter().position(|x| x == sym).unwrap();
        self.bound.remove(idx);
    }
}

impl Visitor for FreeFinder {
    fn visit_abt(&mut self, abt: &mut ABT<Term>) -> bool {
        match abt {
            ABT::Var(symbol, _usage) => {
                self.lookup(&symbol);
            }
            ABT::Abs(sym, _uses, _inner) => {
                self.add(&sym);
            }
            _ => (),
        }
        true
    }
    fn post_abt(&mut self, abt: &mut ABT<Term>) {
        match abt {
            ABT::Abs(sym, _uses, _inner) => {
                self.rm(&sym);
            }
            _ => (),
        }
    }

    fn visit_term(&mut self, _term: &mut Term) -> bool {
        true
    }
}

impl Visitor for Bindings {
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
                let mut finder = FreeFinder::new();
                i.accept(&mut finder);
                info!("Lambda: {:?}", i);
                info!("FreeFinder: {:?} - {:?}", finder.free, finder.bound);
                let mut inner = Bindings::new();

                let mut frees = vec![];
                // It is very important that this ordering be stable.
                // and that the free variables are reported in first-appearence
                // order.
                for sym in finder.free.iter_mut() {
                    // Register the free variable in the parent context
                    *sym = self.lookup(&sym);
                    // let usage = self.usage(&sym);
                    // // This is so we know what to send it.
                    // free.push((sym.clone(), usage, 0, false));
                    // Set up the inner context with it already defined.
                    let mut internal = Symbol {
                        text: sym.text.clone(),
                        num: 0,
                        unique: 0,
                    };
                    inner.add(&mut internal);
                    frees.push((sym.clone(), internal, self.usage(&sym)));
                }

                info!("Lam free: {:?}", free);
                i.accept(&mut inner);

                for (sym, internal, external_usage) in frees.into_iter() {
                    let internal_usage = inner.usage(&internal);
                    free.push((sym, external_usage, internal_usage, false));
                }

                // let current = self.usages.clone();
                // i.accept(self);
                // for (k, pre) in current.iter() {
                //     let post = self.usages.get(k).unwrap();
                //     if post != pre {
                //         free.push((k.clone(), *pre, *post));
                //     }
                // }
                false
            }
            // STOPSHIP here we should reset the usage count for each arm of a Match block, and then take the max ....
            _ => true,
        }
    }
}
