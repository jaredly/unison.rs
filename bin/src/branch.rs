use crate::parser;
use log::info;
use shared::types;
use shared::types::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Causal<Contents> {
    One(Contents),
    Cons(Hash, Contents),
    Merge(Vec<Hash>, Contents),
}

fn resolve_branch(branch: Causal<RawBranch>) -> std::io::Result<RawBranch> {
    match branch {
        Causal::One(c) => Ok(c),
        Causal::Cons(_, b) => Ok(b),
        Causal::Merge(_, b) => Ok(b),
    }
}

#[derive(Debug, Clone)]
pub struct RawBranch {
    pub terms: Star<Referent, NameSegment>,
    pub types: Star<Reference, NameSegment>,
    pub children: HashMap<NameSegment, Hash>,
    pub edits: HashMap<NameSegment, Hash>,
}

impl RawBranch {
    pub fn collect_some_types(
        &self,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<String, Vec<Vec<String>>>,
    ) {
        let mut children: Vec<(&Reference, &NameSegment)> = self.types.d1.iter().collect();
        children.sort();
        for (k, v) in children {
            match k {
                types::Reference::DerivedId(types::Id(hash, _, _)) => {
                    if names.contains_key(&hash.0) {
                        let mut full = path.clone();
                        full.push(v.text.clone());
                        names.get_mut(&hash.0).unwrap().push(full);
                    }
                }
                _ => (),
            }
        }
    }

    pub fn collect_types(
        &self,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
    ) {
        let mut children: Vec<(&Reference, &NameSegment)> = self.types.d1.iter().collect();
        children.sort();
        for (k, v) in children {
            match k {
                types::Reference::DerivedId(types::Id(hash, _, _)) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if names.contains_key(hash) {
                        names.get_mut(hash).unwrap().push(full);
                    } else {
                        names.insert(hash.clone(), vec![full]);
                    }
                }
                _ => (),
            }
        }
    }

    pub fn get_names(
        &self,
    ) -> (
        Vec<(String, String)>,
        Vec<String>,
        Vec<String>,
        Vec<(String, String, usize)>,
    ) {
        let mut terms = vec![];
        let mut constrs = vec![];
        for (k, v) in self.terms.d1.iter() {
            match k {
                types::Referent::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                    terms.push((v.text.clone(), hash.to_string()));
                }
                types::Referent::Con(Reference::DerivedId(Id(hash, _, _)), n, _) => {
                    constrs.push((v.text.clone(), hash.to_string(), *n));
                }
                _ => (),
            }
        }
        (
            terms,
            self.children.keys().map(|k| k.text.clone()).collect(),
            self.types.d1.values().map(|k| k.text.clone()).collect(),
            constrs,
        )
    }

    pub fn collect_terms_and_constructors(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
        constructors: &mut std::collections::HashMap<types::Hash, HashMap<usize, Vec<Vec<String>>>>,
    ) {
        for (k, v) in self.terms.d1.iter() {
            match k {
                types::Referent::Con(types::Reference::DerivedId(types::Id(hash, _, _)), n, _) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if constructors.contains_key(hash) {
                        let m = constructors.get_mut(hash).unwrap();
                        if m.contains_key(n) {
                            m.get_mut(n).unwrap().push(full);
                        } else {
                            m.insert(*n, vec![full]);
                        }
                    } else {
                        let mut m = HashMap::new();
                        m.insert(*n, vec![full]);
                        constructors.insert(hash.clone(), m);
                    }
                }
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if dest.contains_key(hash) {
                        dest.get_mut(hash).unwrap().push(full);
                    } else {
                        dest.insert(hash.clone(), vec![full]);
                    }
                }
                _ => (),
            }
        }
    }

    pub fn collect_terms(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<Vec<String>, types::Hash>,
    ) {
        let mut children: Vec<(&Referent, &NameSegment)> = self.terms.d1.iter().collect();
        children.sort();
        for (k, v) in children {
            match k {
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    dest.insert(full, hash.clone());
                }
                _ => (),
            }
        }
    }

    pub fn collect_names(&self, path: &Vec<String>, names: &mut crate::pack::Names<Hash>) {
        let mut children: Vec<(&Reference, &NameSegment)> = self.types.d1.iter().collect();
        children.sort();
        for (k, v) in children {
            match k {
                types::Reference::DerivedId(types::Id(hash, _, _)) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if names.types.contains_key(hash) {
                        names.types.get_mut(hash).unwrap().push(full);
                    } else {
                        names.types.insert(hash.clone(), vec![full]);
                    }
                }
                _ => (),
            }
        }

        for (k, v) in self.terms.d1.iter() {
            match k {
                types::Referent::Con(types::Reference::DerivedId(types::Id(hash, _, _)), n, _) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if names.constrs.contains_key(hash) {
                        let m = names.constrs.get_mut(hash).unwrap();
                        if m.contains_key(n) {
                            m.get_mut(n).unwrap().push(full);
                        } else {
                            m.insert(*n, vec![full]);
                        }
                    } else {
                        let mut m = HashMap::new();
                        m.insert(*n, vec![full]);
                        names.constrs.insert(hash.clone(), m);
                    }
                }
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    if names.terms.contains_key(hash) {
                        names.terms.get_mut(hash).unwrap().push(full);
                    } else {
                        names.terms.insert(hash.clone(), vec![full]);
                    }
                }
                _ => (),
            }
        }
    }

    pub fn get_flat_names(&self, path: &Vec<String>, dest: &mut crate::printer::FlatNames) {
        for (k, v) in self.terms.d1.iter() {
            match k {
                types::Referent::Con(types::Reference::DerivedId(types::Id(hash, _, _)), n, _) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    let k = (hash.to_string(), *n);
                    match dest.constructors.get(&k) {
                        Some(v) if v.len() < full.len() => (),
                        _ => {
                            dest.constructors.insert(k, full);
                        }
                    };
                }
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    let k = hash.to_string();
                    match dest.terms.get(&k) {
                        Some(v) if v.len() < full.len() => (),
                        _ => {
                            dest.terms.insert(k, full);
                        }
                    };
                }
                _ => (),
            }
        }
        for (k, v) in self.types.d1.iter() {
            match k {
                types::Reference::DerivedId(types::Id(hash, _, _)) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());

                    let k = hash.to_string();
                    match dest.types.get(&k) {
                        Some(v) if v.len() < full.len() => (),
                        _ => {
                            dest.terms.insert(k, full);
                        }
                    };
                }
                _ => (),
            }
        }
    }
}

pub fn default_root() -> std::path::PathBuf {
    let mut project: std::path::PathBuf = std::env::var("HOME").unwrap().into();
    project.push(".unison");
    project.push("v1");
    project
}

pub fn get_head(root: &std::path::Path) -> std::io::Result<String> {
    let head = head_dir(root);
    let entries = std::fs::read_dir(head.as_path())?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
    let name = entries[0].file_name().unwrap().to_str().unwrap().to_owned();
    Ok(name)
}

pub fn head_dir(root: &std::path::Path) -> std::path::PathBuf {
    let root = std::path::PathBuf::from(root);
    let mut head = root.clone();
    head.push("paths");
    head.push("_head");
    head
}

#[derive(Debug, Clone)]
pub struct Codebase {
    branches: HashMap<String, RawBranch>,
    pub head: String,
    paths_root: std::path::PathBuf,
}

impl Codebase {
    pub fn default() -> std::io::Result<Self> {
        let root = default_root();
        Codebase::new(root)
    }

    pub fn root(&self) -> std::path::PathBuf {
        self.paths_root.parent().unwrap().into()
    }

    pub fn new(root: std::path::PathBuf) -> std::io::Result<Self> {
        let head = get_head(root.as_path())?;
        let mut paths_root = root;
        paths_root.push("paths");
        let mut me = Codebase {
            branches: HashMap::new(),
            head: head.clone(),
            paths_root,
        };
        me.load(&head)?;
        Ok(me)
    }

    pub fn terms_and_children(
        &mut self,
        of: &str,
    ) -> std::io::Result<(
        Vec<(String, String)>,
        Vec<String>,
        Vec<String>,
        Vec<(String, String, usize)>,
    )> {
        self.load(of).map(|branch| branch.get_names())
    }

    pub fn reload(&mut self) -> std::io::Result<()> {
        let head = get_head(self.paths_root.parent().unwrap())?;
        self.set_head(head)
    }

    pub fn set_head(&mut self, head: String) -> std::io::Result<()> {
        if head != self.head {
            self.load(&head)?;
            self.head = head;
        }
        Ok(())
    }

    pub fn load(&mut self, hash: &str) -> std::io::Result<&RawBranch> {
        if !self.branches.contains_key(hash) {
            info!("Loading branch : {}", hash);
            let mut head = self.paths_root.clone();
            head.push(hash.to_owned() + ".ub");
            let head = parser::Buffer::from_file(head.as_path())?.get_branch();
            let branch = resolve_branch(head)?;

            self.branches.insert(hash.to_owned(), branch);
        }
        Ok(self.branches.get(hash).unwrap())
    }

    pub fn load_all(&mut self) -> std::io::Result<()> {
        self.load_children(&self.head.clone(), true)
    }

    pub fn load_children(&mut self, of: &str, deep: bool) -> std::io::Result<()> {
        for v in self
            .load(of)?
            .children
            .values()
            .cloned()
            .collect::<Vec<Hash>>()
        {
            self.load(&v.0)?;
            if deep {
                self.load_children(&v.0, deep)?;
            }
        }
        Ok(())
    }

    pub fn get_names(&mut self) -> crate::pack::Names<Hash> {
        let mut dest = Default::default();
        self.collect_names(&self.head.clone(), &vec![], &mut dest);
        dest
    }

    pub fn collect_some_types(
        &self,
        of: &str,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<String, Vec<Vec<String>>>,
    ) {
        let item = self.branches.get(of).unwrap();
        item.collect_some_types(path, names);
        for (k, v) in &item.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            self.collect_some_types(&v.0, &full, names);
        }
    }

    pub fn collect_types(
        &self,
        of: &str,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<Hash, Vec<Vec<String>>>,
    ) {
        let item = self.branches.get(of).unwrap();
        item.collect_types(path, names);
        for (k, v) in &item.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            self.collect_types(&v.0, &full, names);
        }
    }

    pub fn collect_names(
        &mut self,
        of: &str,
        path: &Vec<String>,
        dest: &mut crate::pack::Names<Hash>,
    ) {
        let item = self.load(of).unwrap();
        item.collect_names(path, dest);
        for (k, v) in &item.children.clone() {
            let mut full = path.clone();
            full.push(k.text.clone());
            self.collect_names(&v.0, &full, dest);
        }
    }

    pub fn collect_terms(
        &self,
        of: &str,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<Vec<String>, types::Hash>,
    ) {
        let item = self.branches.get(of).unwrap();
        item.collect_terms(path, dest);
        let mut children: Vec<(&NameSegment, &Hash)> = item.children.iter().collect();
        children.sort_by(|(a, _), (b, _)| NameSegment::cmp(a, b));
        for (k, v) in children {
            let mut full = path.clone();
            full.push(k.text.clone());
            self.collect_terms(&v.0, &full, dest);
        }
    }

    pub fn find_ns(&mut self, path: &[&str]) -> std::io::Result<Hash> {
        self.find_ns_inner(&self.head.clone(), path)
    }

    fn find_ns_inner(&mut self, of: &str, path: &[&str]) -> std::io::Result<Hash> {
        let seg = NameSegment {
            text: path[0].to_owned(),
        };
        let item = self.load(of)?;
        // self.load_child(of, path[0])?;
        let child = item
            .children
            .get(&seg)
            .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?
            .clone();
        if path.len() == 1 {
            Ok(child)
        } else {
            return self.find_ns_inner(&child.0, &path[1..]);
        }
    }

    pub fn find_term(&mut self, path: &[&str]) -> std::io::Result<Hash> {
        // TODO refactor to just use find_ns_inner
        self.find_term_inner(&self.head.clone(), path)
    }

    fn find_term_inner(&mut self, of: &str, path: &[&str]) -> std::io::Result<Hash> {
        let seg = NameSegment {
            text: path[0].to_owned(),
        };
        let item = self.load(of)?;
        if path.len() == 1 {
            for (k, v) in item.terms.d1.iter() {
                if v.text == path[0] {
                    println!("{:?} : {:?}", k, v);
                    return Ok(match k.reference() {
                        Reference::Builtin(_) => unreachable!(),
                        Reference::DerivedId(Id(hash, _, _)) => hash.clone(),
                    });
                }
            }
            return Err(std::io::ErrorKind::NotFound.into());
        } else {
            // self.load_child(of, path[0])?;
            let child = item
                .children
                .get(&seg)
                .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?
                .clone();
            return self.find_term_inner(&child.0, &path[1..]);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub raw: RawBranch,
    pub children: HashMap<NameSegment, Branch>,
}

impl Branch {
    pub fn load(paths_root: &std::path::PathBuf, hash: String) -> std::io::Result<Branch> {
        info!("Loading branch : {}", hash);
        let mut head = paths_root.clone();
        head.push(hash + ".ub");
        let head = parser::Buffer::from_file(head.as_path())?.get_branch();
        let head = resolve_branch(head)?;
        Ok(Branch {
            raw: head,
            children: std::collections::HashMap::new(),
        })
    }

    pub fn load_child(
        &mut self,
        paths_root: &std::path::PathBuf,
        child: &str,
    ) -> std::io::Result<()> {
        info!("Loading child : {}", child);
        let seg = NameSegment {
            text: child.to_owned(),
        };
        if self.children.contains_key(&seg) {
            return Ok(());
        }
        let hash = self
            .raw
            .children
            .get(&seg)
            .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?;
        self.children
            .insert(seg, Branch::load(paths_root, hash.to_string())?);
        Ok(())
    }

    pub fn load_children(
        &mut self,
        paths_root: &std::path::PathBuf,
        deep: bool,
    ) -> std::io::Result<()> {
        let mut children: Vec<(&NameSegment, &Hash)> = self.raw.children.iter().collect();
        children.sort();
        for (k, v) in children {
            let mut child = Branch::load(paths_root, v.to_string())?;
            if deep {
                child.load_children(paths_root, deep)?;
            }
            self.children.insert(k.clone(), child);
        }
        Ok(())
    }

    pub fn collect_types(
        &self,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
    ) {
        self.raw.collect_types(path, names);
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.collect_types(&full, names);
        }
    }

    pub fn collect_terms_and_constructors(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
        constructors: &mut std::collections::HashMap<types::Hash, HashMap<usize, Vec<Vec<String>>>>,
    ) {
        self.raw
            .collect_terms_and_constructors(path, dest, constructors);
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.collect_terms_and_constructors(&full, dest, constructors);
        }
    }

    pub fn get_flat_names(&self, path: &Vec<String>, dest: &mut crate::printer::FlatNames) {
        self.raw.get_flat_names(path, dest);
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.get_flat_names(&full, dest);
        }
    }

    pub fn find_term(
        &mut self,
        paths_root: &std::path::PathBuf,
        path: &[&str],
    ) -> std::io::Result<Hash> {
        // let mut parts: Vec<&str> = path.split(".").collect();
        // if parts[0] == "." {
        //     parts.remove(0);
        // }
        let seg = NameSegment {
            text: path[0].to_owned(),
        };
        if path.len() == 1 {
            for (k, v) in self.raw.terms.d1.iter() {
                if v.text == path[0] {
                    return Ok(match k.reference() {
                        Reference::Builtin(_) => unreachable!(),
                        Reference::DerivedId(Id(hash, _, _)) => hash.clone(),
                    });
                }
            }
            return Err(std::io::ErrorKind::NotFound.into());
        } else {
            self.load_child(paths_root, path[0])?;
            let child = self
                .children
                .get_mut(&seg)
                .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?;
            return child.find_term(paths_root, &path[1..]);
        }
    }

    pub fn collect_terms(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<Vec<String>, types::Hash>,
    ) {
        let mut children: Vec<(&Referent, &NameSegment)> = self.raw.terms.d1.iter().collect();
        children.sort();
        for (k, v) in children {
            match k {
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    dest.insert(full, hash.clone());
                }
                _ => (),
            }
        }
        let mut children: Vec<(&NameSegment, &Branch)> = self.children.iter().collect();
        children.sort_by(|(a, _), (b, _)| NameSegment::cmp(a, b));
        for (k, v) in children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.collect_terms(&full, dest);
        }
    }
}
