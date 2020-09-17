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
        serde_json::to_string(&all_branch_names(&branch)).unwrap(),
    )?;

    Ok(())
}

fn all_branch_names(
    branch: &Branch,
) -> (
    HashMap<String, Vec<Vec<String>>>,
    HashMap<String, HashMap<usize, Vec<Vec<String>>>>,
    HashMap<String, Vec<Vec<String>>>,
) {
    let (all_names, all_constr_names, all_type_names) = branch_names(branch);
    use std::iter::FromIterator;
    let term_names = HashMap::from_iter(all_names.iter().map(|(k, v)| (k.to_string(), v.clone())));
    let constr_names = HashMap::from_iter(
        all_constr_names
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone())),
    );
    let type_names = HashMap::from_iter(
        all_type_names
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone())),
    );

    (term_names, constr_names, type_names)
}

fn branch_names(
    branch: &Branch,
) -> (
    HashMap<Hash, Vec<Vec<String>>>,
    HashMap<Hash, HashMap<usize, Vec<Vec<String>>>>,
    HashMap<Hash, Vec<Vec<String>>>,
) {
    let mut all_names = HashMap::new();
    let mut all_constr_names = HashMap::new();
    branch.collect_names(&vec![], &mut all_names, &mut all_constr_names);
    let mut all_type_names = HashMap::new();
    branch.type_names(&vec![], &mut all_type_names);
    (all_names, all_constr_names, all_type_names)
}

fn env_names(
    branch: &Branch,
    runtime_env: &RuntimeEnv,
) -> (
    HashMap<String, Vec<Vec<String>>>,
    HashMap<String, HashMap<usize, Vec<Vec<String>>>>,
    HashMap<String, Vec<Vec<String>>>,
) {
    let (all_names, all_constr_names, all_type_names) = branch_names(branch);

    // let mut all_names = HashMap::new();
    // let mut all_constr_names = HashMap::new();
    // branch.collect_names(&vec![], &mut all_names, &mut all_constr_names);
    // let mut all_type_names = HashMap::new();
    // branch.type_names(&vec![], &mut all_type_names);

    let mut term_names = HashMap::new();
    let mut constr_names = HashMap::new();
    for hash in runtime_env.terms.keys() {
        if all_names.contains_key(hash) {
            term_names.insert(hash.to_string(), all_names.get(hash).unwrap().clone());
        }
        if all_constr_names.contains_key(hash) {
            constr_names.insert(
                hash.to_string(),
                all_constr_names.get(hash).unwrap().clone(),
            );
        }
    }

    let mut type_names = HashMap::new();
    for hash in runtime_env.types.keys() {
        if all_type_names.contains_key(hash) {
            type_names.insert(hash.to_string(), all_type_names.get(hash).unwrap().clone());
        }
    }

    (term_names, constr_names, type_names)
}

pub fn term_to_env(
    root: &std::path::Path,
    hash: &str,
    _out: &str,
) -> std::io::Result<ir::TranslationEnv> {
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

    Ok(ir_env)
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

pub fn pack_term_json(terms_path: &std::path::Path, hash: &str, out: &str) -> std::io::Result<()> {
    let root = terms_path.parent().unwrap();
    let runtime_env = term_to_env(root, hash, out)?.into();

    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    std::fs::write(
        out.to_owned() + ".names.json",
        serde_json::to_string(&env_names(&branch, &runtime_env)).unwrap(),
    )?;

    std::fs::write(
        out,
        serde_json::to_string(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    )
}

pub fn pack_term(terms_path: &std::path::Path, hash: &str, out: &str) -> std::io::Result<()> {
    let root = terms_path.parent().unwrap();
    let mut ir_env = term_to_env(root, hash, out)?;

    {
        let mut walker = TypeWalker(&mut ir_env.env);
        let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
        for k in ks {
            use visitor::Accept;
            let mut m = walker.0.load_type(&k);
            m.accept(&mut walker);
        }
    }

    let runtime_env = ir_env.into();

    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    std::fs::write(out, shared::pack(&runtime_env))?;
    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string(&env_names(&branch, &runtime_env)).unwrap(),
    )?;

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
        serde_json::to_string(&all_branch_names(&branch)).unwrap(),
    )?;
    std::fs::write(
        outfile,
        serde_json::to_string(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    )?;

    Ok(())

    // pack_all_terms_json(
    //     path.parent().unwrap(),
    //     &path.file_name().unwrap().to_str().unwrap()[1..],
    //     outfile,
    // )?;

    // fn pack_term_json(terms_path: &std::path::Path, hash: &str, out: &str) -> std::io::Result<()> {
    // let root = terms_path.parent().unwrap();
    // let runtime_env = term_to_env(root, hash, out)?;

    // let paths = path_with(&root, "paths");
    // let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    // branch.load_children(&paths, true)?;

    // std::fs::write(
    //     out.to_owned() + ".names.json",
    //     serde_json::to_string(&env_names(&branch, &runtime_env)).unwrap(),
    // )?;

    // std::fs::write(
    //     out,
    //     serde_json::to_string(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    // )
    // }

    // return Ok(());
}

pub fn pack_json(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    pack_term_json(
        path.parent().unwrap(),
        &path.file_name().unwrap().to_str().unwrap()[1..],
        outfile,
    )?;
    return Ok(());
}

pub fn pack(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    pack_term(
        path.parent().unwrap(),
        &path.file_name().unwrap().to_str().unwrap()[1..],
        outfile,
    )?;
    return Ok(());
}
