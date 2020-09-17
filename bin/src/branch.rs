use crate::parser;
use shared::types;
use shared::types::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub enum Causal<Contents> {
    One(Contents),
    Cons(Hash, Contents),
    Merge(Vec<Hash>, Contents),
}

fn resolve_branch(
    _root: &std::path::Path,
    branch: Causal<RawBranch>,
) -> std::io::Result<RawBranch> {
    match branch {
        Causal::One(c) => Ok(c),
        Causal::Cons(_, b) => {
            // let mut p = std::path::PathBuf::from(root);
            // p.push(h.to_string() + ".ub");
            // // println!("Resolve: {:?}", p);
            // b.merge(&resolve_branch(
            //     root,
            //     parser::Buffer::from_file(p.as_path())?.get_branch(),
            // )?);
            Ok(b)
        }
        Causal::Merge(_, b) => {
            // for hash in hashes.iter() {
            //     let mut p = std::path::PathBuf::from(root);
            //     p.push(hash.to_string() + ".ub");
            //     b.merge(&resolve_branch(
            //         root,
            //         parser::Buffer::from_file(p.as_path())?.get_branch(),
            //     )?)
            // }
            Ok(b)
        }
    }
}

#[derive(Debug, Clone)]
pub struct RawBranch {
    pub terms: Star<Referent, NameSegment>,
    pub types: Star<Reference, NameSegment>,
    pub children: HashMap<NameSegment, Hash>,
    pub edits: HashMap<NameSegment, Hash>,
}

#[derive(Debug, Clone)]
pub struct Branch {
    pub raw: RawBranch,
    pub children: HashMap<NameSegment, Branch>,
}

impl Branch {
    pub fn load(root: &std::path::PathBuf, hash: String) -> std::io::Result<Branch> {
        let mut head = root.clone();
        head.push(hash + ".ub");
        // println!("Yes: {:?}", head);
        let head = parser::Buffer::from_file(head.as_path())?.get_branch();
        let head = resolve_branch(&root, head)?;
        Ok(Branch {
            raw: head,
            children: std::collections::HashMap::new(),
        })
    }

    pub fn load_child(&mut self, root: &std::path::PathBuf, child: &str) -> std::io::Result<()> {
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
            .insert(seg, Branch::load(root, hash.to_string())?);
        Ok(())
    }

    pub fn load_children(&mut self, root: &std::path::PathBuf, deep: bool) -> std::io::Result<()> {
        let mut children: Vec<(&NameSegment, &Hash)> = self.raw.children.iter().collect();
        children.sort();
        for (k, v) in children {
            let mut child = Branch::load(root, v.to_string())?;
            if deep {
                child.load_children(root, deep)?;
            }
            self.children.insert(k.clone(), child);
        }
        Ok(())
    }

    pub fn type_names(
        &self,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
    ) {
        let mut children: Vec<(&Reference, &NameSegment)> = self.raw.types.d1.iter().collect();
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
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.type_names(&full, names);
        }
    }

    pub fn get_names(&self, path: &Vec<String>, dest: &mut crate::printer::Names) {
        for (k, v) in self.raw.terms.d1.iter() {
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
        for (k, v) in self.raw.types.d1.iter() {
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
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.get_names(&full, dest);
        }
    }

    pub fn collect_names(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
        constructors: &mut std::collections::HashMap<types::Hash, HashMap<usize, Vec<Vec<String>>>>,
    ) {
        for (k, v) in self.raw.terms.d1.iter() {
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
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.collect_names(&full, dest, constructors);
        }
    }

    pub fn find_term(&mut self, root: &std::path::PathBuf, path: &[&str]) -> std::io::Result<Hash> {
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
            self.load_child(root, path[0])?;
            let child = self
                .children
                .get_mut(&seg)
                .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?;
            return child.find_term(root, &path[1..]);
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
