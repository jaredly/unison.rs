use super::parser;
use crate::visitor::Accept;
use log::info;
use shared::types::*;
use std::collections::HashMap;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    TermNotFound(String),
    NotImplemented(String),
}

fn concat(mut buf: std::path::PathBuf, thing: String) -> std::path::PathBuf {
    buf.push(thing);
    return buf;
}

#[derive(Clone, Debug)]
pub struct Env {
    // stack: Vec<Frame>,
    pub root: std::path::PathBuf,
    pub term_cache: HashMap<String, (ABT<Term>, ABT<Type>)>,
    pub type_cache: HashMap<String, TypeDecl>,
}

impl Env {
    pub fn init(root: &std::path::Path) -> Self {
        Env {
            root: std::path::PathBuf::from(root),
            term_cache: HashMap::new(),
            type_cache: HashMap::new(),
        }
    }

    pub fn has_type(&self, hash: &str) -> bool {
        self.type_cache.contains_key(hash)
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

    pub fn load(&mut self, hash: &str) -> Result<(ABT<Term>, ABT<Type>)> {
        match self.term_cache.get(hash) {
            Some(v) => Ok(v.clone()),
            None => {
                let mut base = self.root.clone();
                base.push("terms");
                base.push("#".to_owned() + hash);
                // full.push("compiled.ub");
                info!("Trying to load term {:?}", base);
                let mut f = concat(base.clone(), "compiled.ub".into());
                let mut file = f.as_path();
                if !file.exists() {
                    // yay wonky hack
                    // full = self.root.clone();
                    // full.push("terms");
                    // full.push("compiled.ub");
                    base.pop();
                    base.push(format!("#{}.0c3", hash));

                    f = concat(base.clone(), "compiled.ub".into());
                    file = f.as_path();
                    if !file.exists() {
                        println!("Didnt work also {:?}", file);
                        // unreachable!("Nope");
                        return Err(Error::TermNotFound(hash.to_owned()));
                    }
                }
                let type_file = {
                    // let mut full = self.root.clone();
                    // full.push("terms");
                    // full.push("#".to_owned() + hash);
                    // full.push("type.ub");
                    // full
                    concat(base.clone(), "type.ub".into())
                };
                let typ = parser::Buffer::from_file(type_file.as_path())
                    .unwrap()
                    .get_term_type();

                let mut res = parser::Buffer::from_file(file).unwrap().get_term();

                // This adds unique identifiers to symbols
                let mut bindings = super::unique::Bindings::new();
                res.accept(&mut bindings);

                self.term_cache
                    .insert(hash.to_owned(), (res.clone(), typ.clone()));
                Ok((res, typ))
            }
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
