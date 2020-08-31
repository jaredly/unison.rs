use super::parser;
use crate::visitor::Accept;
use log::info;
use shared::types::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Env {
    // stack: Vec<Frame>,
    pub root: std::path::PathBuf,
    pub raw_cache: HashMap<String, ABT<Term>>,
    pub cache: HashMap<String, Term>,
    pub type_cache: HashMap<String, TypeDecl>,
}

impl Env {
    pub fn init(root: &std::path::Path) -> Self {
        Env {
            root: std::path::PathBuf::from(root),
            raw_cache: HashMap::new(),
            cache: HashMap::new(),
            type_cache: HashMap::new(),
        }
    }

    pub fn load_type(&mut self, hash: &str) -> TypeDecl {
        match self.type_cache.get(hash) {
            Some(v) => v.clone(),
            None => {
                let mut full = self.root.clone();
                full.push("types");
                full.push("#".to_owned() + hash);
                full.push("compiled.ub");
                info!("Trying to load type {:?}", full);
                let res = parser::Buffer::from_file(full.as_path())
                    .unwrap()
                    .get_type();
                self.type_cache.insert(hash.to_owned(), res.clone());
                res
            }
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
                    full.push("#".to_owned() + hash);
                    full.push("compiled.ub");
                    info!("Trying to load {:?}", full);
                    let file = full.as_path();
                    if !file.exists() {
                        unreachable!("Not a good hash folks {}", hash);
                    }
                    let mut res = parser::Buffer::from_file(file).unwrap().get_term();
                    let mut bindings = super::unique::Bindings::new();
                    // use super::visitor::Visitor;
                    // bindings.visit_abt(&mut res);
                    res.accept(&mut bindings);
                    // res.unique(&mut bindings);
                    self.raw_cache.insert(hash.to_owned(), res.clone());
                    res
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct Stack(pub Vec<Frame>);

#[derive(Clone, Debug)]
pub struct Frame {
    pub term: String, // TODO Hash
    pub moment: usize,
    pub bindings: Vec<(String, Term)>,
}
