use super::parser;
use super::types::*;
use log::info;
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
        // let root = path.parent().unwrap().parent();
        // let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        Env {
            // stack: vec![],
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
                info!("Trying to load {:?}", full);
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
    pub fn lookup(&self, name: &str) -> Term {
        for (n, term) in &self.0[0].bindings {
            if n == name {
                return term.clone();
            }
        }
        unreachable!("No term {} found - {}", name, self.0[0].term);
    }

    pub fn push(&mut self, k: String, v: Term) {
        info!(
            "---- Stack push({}) {} {:?}",
            self.0[0].bindings.len(),
            k,
            v
        );
        self.0[0].bindings.push((k, v));
    }

    pub fn set(&mut self, k: String, v: Term) {
        info!("---- Stack set({}) {} {:?}", self.0[0].bindings.len(), k, v);
        self.0[0].bindings.insert(0, (k, v));
    }

    pub fn with(&self, k: String, v: Term) -> Self {
        let mut nw = self.clone();
        nw.set(k, v);
        nw
    }

    pub fn with_frame(&self, hash: String) -> Self {
        let mut nw = self.clone();
        nw.0.insert(0, Frame::new(hash));
        nw
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub term: String, // TODO Hash
    pub moment: usize,
    pub bindings: Vec<(String, Term)>,
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
