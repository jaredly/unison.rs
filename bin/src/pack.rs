use crate::branch::Branch;
use crate::env;
use crate::ir;
use crate::visitor;
use serde_derive::Serialize;
use shared::types;
use shared::types::*;
use std::collections::HashMap;

struct TypeWalker<'a>(&'a mut crate::env::Env);

pub fn path_with(path: &std::path::Path, part: &str) -> std::path::PathBuf {
    let mut m = std::path::PathBuf::from(path);
    m.push(part);
    m
}

impl<'a> visitor::Visitor for TypeWalker<'a> {
    fn visit_abt<Inner: visitor::Accept>(&mut self, _: &mut ABT<Inner>) -> bool {
        true
    }

    fn visit_term(&mut self, term: &mut Term) -> bool {
        match term {
            Term::Constructor(Reference::DerivedId(Id(hash, _, _)), _) => {
                let hash = hash.to_string();
                if !self.0.has_type(&hash) {
                    use visitor::Accept;
                    self.0.load_type(&hash).accept(self);
                }
            }
            _ => (),
        };
        true
    }

    fn visit_type(&mut self, typ: &mut Type) -> bool {
        match typ {
            Type::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                let hash = hash.to_string();
                if !self.0.has_type(&hash) {
                    use visitor::Accept;
                    self.0.load_type(&hash).accept(self);
                }
            }
            _ => (),
        }
        true
    }
    fn post_abt<Inner: visitor::Accept>(&mut self, _: &mut ABT<Inner>) {}
}

pub fn get_head(root: &std::path::Path) -> std::io::Result<String> {
    let root = std::path::PathBuf::from(root);
    let mut head = root.clone();
    head.push("_head");
    let entries = std::fs::read_dir(head.as_path())?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
    let name = entries[0].file_name().unwrap().to_str().unwrap().to_owned();
    Ok(name)
}

pub fn pack_all(terms_path: &std::path::Path, out: &str) -> std::io::Result<()> {
    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    let mut all_terms = std::collections::HashMap::new();
    branch.collect_terms(&vec![], &mut all_terms);

    let env = env::Env::init(root);
    let mut ir_env = ir::TranslationEnv::new(env);

    let mut hashes: Vec<&Hash> = all_terms.values().collect();
    hashes.sort();
    for hash in hashes {
        let _ = ir_env.load(hash);
    }

    {
        let mut walker = TypeWalker(&mut ir_env.env);
        let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
        for k in ks {
            use visitor::Accept;
            let mut m = walker.0.load_type(&k);
            m.accept(&mut walker);
        }
    }

    let runtime_env: shared::types::RuntimeEnv = ir_env.into();

    std::fs::write(out, shared::pack(&runtime_env))?;
    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string(&branch_names(&branch).serialize()).unwrap(),
    )?;

    Ok(())
}

struct Names<T: ToString> {
    terms: HashMap<T, Vec<Vec<String>>>,
    constrs: HashMap<T, HashMap<usize, Vec<Vec<String>>>>,
    types: HashMap<T, Vec<Vec<String>>>,
}

impl<T: ToString> Names<T> {
    fn serialize(
        self,
    ) -> (
        HashMap<String, Vec<Vec<String>>>,
        HashMap<String, HashMap<usize, Vec<Vec<String>>>>,
        HashMap<String, Vec<Vec<String>>>,
    ) {
        use std::iter::FromIterator;
        let term_names =
            HashMap::from_iter(self.terms.iter().map(|(k, v)| (k.to_string(), v.clone())));
        let constr_names =
            HashMap::from_iter(self.constrs.iter().map(|(k, v)| (k.to_string(), v.clone())));
        let type_names =
            HashMap::from_iter(self.types.iter().map(|(k, v)| (k.to_string(), v.clone())));
        (term_names, constr_names, type_names)
    }
}

fn branch_names(branch: &Branch) -> Names<Hash> {
    let mut all_names = HashMap::new();
    let mut all_constr_names = HashMap::new();
    branch.collect_names(&vec![], &mut all_names, &mut all_constr_names);
    let mut all_type_names = HashMap::new();
    branch.type_names(&vec![], &mut all_type_names);
    Names {
        terms: all_names,
        constrs: all_constr_names,
        types: all_type_names,
    }
}

fn env_names(branch: &Branch, runtime_env: &RuntimeEnv) -> Names<String> {
    let names = branch_names(branch);

    let mut term_names = HashMap::new();
    let mut constr_names = HashMap::new();
    for hash in runtime_env.terms.keys() {
        if names.terms.contains_key(hash) {
            term_names.insert(hash.to_string(), names.terms.get(hash).unwrap().clone());
        }
    }

    let mut type_names = HashMap::new();
    for hash in runtime_env.types.keys() {
        if names.types.contains_key(hash) {
            type_names.insert(hash.to_string(), names.types.get(hash).unwrap().clone());
        }
        if names.constrs.contains_key(hash) {
            constr_names.insert(hash.to_string(), names.constrs.get(hash).unwrap().clone());
        }
    }

    Names {
        terms: term_names,
        constrs: constr_names,
        types: type_names,
    }
}

pub fn term_to_env(
    root: &std::path::Path,
    hash: &str,
    _out: &str,
) -> std::io::Result<types::RuntimeEnv> {
    let env = env::Env::init(&root);
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash)).unwrap();

    {
        let mut walker = TypeWalker(&mut ir_env.env);
        let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
        for k in ks {
            use visitor::Accept;
            let mut m = walker.0.load_type(&k);
            m.accept(&mut walker);
        }
    }

    walk_env(&mut ir_env.env);

    Ok(ir_env.into())
}

#[derive(Serialize, Debug)]
struct JsonEnv {
    terms: HashMap<String, (Vec<IR>, ABT<Type>)>,
    types: HashMap<String, TypeDecl>,
    anon_fns: Vec<(Hash, Vec<IR>)>,
}
impl JsonEnv {
    fn from_runtime(
        RuntimeEnv {
            terms,
            types,
            anon_fns,
        }: RuntimeEnv,
    ) -> JsonEnv {
        use std::iter::FromIterator;
        JsonEnv {
            terms: HashMap::from_iter(terms.iter().map(|(k, v)| (k.to_string(), v.clone()))),
            types: HashMap::from_iter(types.iter().map(|(k, v)| (k.to_string(), v.clone()))),
            anon_fns,
        }
    }
}

fn walk_env(env: &mut env::Env) {
    let mut walker = TypeWalker(env);
    let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
    for k in ks {
        use visitor::Accept;
        let mut m = walker.0.load_type(&k);
        m.accept(&mut walker);
    }
}

fn load_main_branch(root: &std::path::Path) -> std::io::Result<Branch> {
    println!("Loading all namespaces");
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;
    println!("Finished loading namespaces");

    Ok(branch)
}

pub fn pack_term_json(
    branch: Branch,
    root: &std::path::Path,
    hash: &str,
    out: &str,
) -> std::io::Result<()> {
    let runtime_env = term_to_env(root, hash, out)?;

    std::fs::write(
        out.to_owned() + ".names.json",
        serde_json::to_string_pretty(&env_names(&branch, &runtime_env).serialize()).unwrap(),
    )?;

    std::fs::write(
        out,
        serde_json::to_string(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    )
}

pub fn pack_term(
    branch: Branch,
    root: &std::path::Path,
    hash: &str,
    out: &str,
) -> std::io::Result<()> {
    let runtime_env = term_to_env(root, hash, out)?;

    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string_pretty(&env_names(&branch, &runtime_env).serialize()).unwrap(),
    )?;

    std::fs::write(out, shared::pack(&runtime_env))?;

    Ok(())
}

pub fn pack_all_json(file: &String, outfile: &String) -> std::io::Result<()> {
    let terms_path = std::path::PathBuf::from(file);

    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    let mut all_terms = std::collections::HashMap::new();
    branch.collect_terms(&vec![], &mut all_terms);

    let env = env::Env::init(root);
    let mut ir_env = ir::TranslationEnv::new(env);

    let mut hashes: Vec<&Hash> = all_terms.values().collect();
    hashes.sort();
    for hash in hashes {
        let _ = ir_env.load(hash);
    }

    {
        let mut walker = TypeWalker(&mut ir_env.env);
        let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
        for k in ks {
            use visitor::Accept;
            let mut m = walker.0.load_type(&k);
            m.accept(&mut walker);
        }
    }

    let runtime_env: shared::types::RuntimeEnv = ir_env.into();

    std::fs::write(
        outfile.to_owned() + ".names.json",
        serde_json::to_string_pretty(&branch_names(&branch).serialize()).unwrap(),
    )?;
    std::fs::write(
        outfile,
        serde_json::to_string_pretty(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    )?;

    Ok(())
}

pub fn default_root() -> std::path::PathBuf {
    let mut project: std::path::PathBuf = std::env::var("HOME").unwrap().into();
    project.push(".unison");
    project.push("v1");
    project
}

fn find_term(root: &std::path::Path, branch: &mut Branch, term: &str) -> Hash {
    let paths = path_with(&root, "paths");
    if &term[0..1] == "." {
        branch
            .find_term(
                &paths,
                &term[1..].split(".").collect::<Vec<&str>>().as_slice(),
            )
            .unwrap()
    } else {
        types::Hash::from_string(term)
    }
}

pub fn pack_json(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);
    if path.exists() {
        let root = path.parent().unwrap().parent().unwrap();
        let branch = load_main_branch(root)?;
        let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        pack_term_json(branch, root, &hash, outfile)?;
    } else {
        let root = default_root();
        let mut branch = load_main_branch(root.as_path())?;
        let hash = find_term(root.as_path(), &mut branch, file);
        pack_term_json(branch, root.as_path(), &hash.0, outfile)?;
    }

    return Ok(());
}

pub fn pack(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.exists() {
        let root = path.parent().unwrap().parent().unwrap();
        let branch = load_main_branch(root)?;
        let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        pack_term(branch, root, &hash, outfile)
    } else {
        let root = default_root();
        let mut branch = load_main_branch(root.as_path())?;
        let hash = find_term(root.as_path(), &mut branch, file);
        pack_term(branch, root.as_path(), &hash.0, outfile)
    }
}
