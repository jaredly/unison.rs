use crate::branch::Codebase;
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

pub fn head_dir(root: &std::path::Path) -> std::path::PathBuf {
    let root = std::path::PathBuf::from(root);
    let mut head = root.clone();
    head.push("paths");
    head.push("_head");
    head
}

pub fn get_head(root: &std::path::Path) -> std::io::Result<String> {
    let head = head_dir(root);
    let entries = std::fs::read_dir(head.as_path())?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
    let name = entries[0].file_name().unwrap().to_str().unwrap().to_owned();
    Ok(name)
}

pub fn pack_all(terms_path: &std::path::Path, out: &str) -> std::io::Result<()> {
    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;

    let mut all_terms = std::collections::HashMap::new();
    codebase.collect_terms(&codebase.head.clone(), &vec![], &mut all_terms);

    let env = env::Env::init(root);
    let mut ir_env = ir::TranslationEnv::new(env);

    let mut hashes: Vec<&Hash> = all_terms.values().collect();
    hashes.sort();
    for hash in hashes {
        println!("Loading {:?}", hash);
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
        serde_json::to_string(&codebase.get_names().serialize()).unwrap(),
    )?;

    Ok(())
}

pub struct Names<T: ToString> {
    pub terms: HashMap<T, Vec<Vec<String>>>,
    pub constrs: HashMap<T, HashMap<usize, Vec<Vec<String>>>>,
    pub types: HashMap<T, Vec<Vec<String>>>,
}

impl Default for Names<Hash> {
    fn default() -> Self {
        Names {
            terms: Default::default(),
            constrs: Default::default(),
            types: Default::default(),
        }
    }
}

impl<T: ToString> Names<T> {
    pub fn serialize(
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

// fn branch_names(codebase: &Codebase) -> Names<Hash> {
//     let mut all_names = HashMap::new();
//     let mut all_constr_names = HashMap::new();
//     codebase.collect_terms_and_constructors(&vec![], &mut all_names, &mut all_constr_names);
//     let mut all_type_names = HashMap::new();
//     codebase.collect_types(&vec![], &mut all_type_names);
//     Names {
//         terms: all_names,
//         constrs: all_constr_names,
//         types: all_type_names,
//     }
// }

pub fn env_names(names: &Names<Hash>, runtime_env: &RuntimeEnv) -> Names<String> {
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

pub fn terms_to_env(
    root: &std::path::Path,
    hashes: Vec<Hash>,
) -> std::io::Result<types::RuntimeEnv> {
    let env = env::Env::init(&root);
    let mut ir_env = ir::TranslationEnv::new(env);
    for hash in hashes {
        ir_env.load(&hash).unwrap();
    }

    walk_env(&mut ir_env.env);

    Ok(ir_env.into())
}

pub fn term_to_env(root: &std::path::Path, hash: &str) -> std::io::Result<types::RuntimeEnv> {
    let env = env::Env::init(&root);
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash)).unwrap();

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

pub fn load_main_branch(root: &std::path::Path) -> std::io::Result<Codebase> {
    println!("Loading all namespaces");
    // let paths = path_with(&root, "paths");
    // let mut codebase = Codebase::load(&paths, get_head(&root)?)?;
    // codebase.load_children(&paths, true)?;
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;
    println!("Finished loading namespaces");

    Ok(codebase)
}

pub fn pack_term_json(
    mut codebase: Codebase,
    root: &std::path::Path,
    hash: &str,
    out: &str,
) -> std::io::Result<()> {
    let runtime_env = term_to_env(root, hash)?;

    std::fs::write(
        out.to_owned() + ".names.json",
        serde_json::to_string_pretty(
            &env_names(&mut codebase.get_names(), &runtime_env).serialize(),
        )
        .unwrap(),
    )?;

    std::fs::write(
        out,
        serde_json::to_string(&JsonEnv::from_runtime(runtime_env)).unwrap(),
    )
}

pub fn pack_term(
    codebase: &mut Codebase,
    root: &std::path::Path,
    hash: &str,
    out: &str,
) -> std::io::Result<()> {
    let runtime_env = term_to_env(root, hash)?;

    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string_pretty(&env_names(&codebase.get_names(), &runtime_env).serialize())
            .unwrap(),
    )?;

    std::fs::write(out, shared::pack(&runtime_env))?;

    Ok(())
}

pub fn pack_all_json(file: &String, ns: &String, outfile: &String) -> std::io::Result<()> {
    let terms_path = std::path::PathBuf::from(file);

    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;

    let mut all_terms = std::collections::HashMap::new();

    // let head = codebase.head.clone();
    // if ns != "." {
    // }
    let ns = if ns == "" || ns == "." {
        codebase.head.clone()
    } else {
        codebase
            .find_ns(ns.split(".").collect::<Vec<&str>>().as_slice())
            .unwrap()
            .0
    };

    codebase.collect_terms(&ns, &vec![], &mut all_terms);

    let env = env::Env::init(root);
    let mut ir_env = ir::TranslationEnv::new(env);

    for hash in all_terms.values() {
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
        serde_json::to_string_pretty(&codebase.get_names().serialize()).unwrap(),
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

pub fn find_term(codebase: &mut Codebase, term: &str) -> Hash {
    if &term[0..1] == "." {
        codebase
            .find_term(&term[1..].split(".").collect::<Vec<&str>>().as_slice())
            .unwrap()
    } else {
        types::Hash::from_string(term)
    }
}

pub fn pack_json(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);
    if path.exists() {
        let root = path.parent().unwrap().parent().unwrap();
        let codebase = load_main_branch(root)?;
        let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        pack_term_json(codebase, root, &hash, outfile)?;
    } else {
        let root = default_root();
        let mut codebase = load_main_branch(root.as_path())?;
        let hash = find_term(&mut codebase, file);
        pack_term_json(codebase, root.as_path(), &hash.0, outfile)?;
    }

    return Ok(());
}

pub fn pack(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.exists() {
        let root = path.parent().unwrap().parent().unwrap();
        let mut codebase = load_main_branch(root)?;
        let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        pack_term(&mut codebase, root, &hash, outfile)
    } else {
        let root = default_root();
        let mut codebase = load_main_branch(root.as_path())?;
        let hash = find_term(&mut codebase, file);
        pack_term(&mut codebase, root.as_path(), &hash.0, outfile)
    }
}

pub fn pack_watch(term: &String, outfile: &String) -> std::io::Result<()> {
    use notify::{RecommendedWatcher, RecursiveMode, Watcher};
    use std::sync::mpsc::channel;
    use std::time::Duration;

    let root = default_root();
    // TODO incremental reload!!!

    let now = std::time::Instant::now();
    println!("Initial build...");
    let mut codebase = load_main_branch(root.as_path())?;
    let hash = find_term(&mut codebase, term);
    pack_term(&mut codebase, root.as_path(), &hash.0, outfile)?;
    println!("Finished! {:?}", now.elapsed());

    // Create a channel to receive the events.
    let (tx, rx) = channel();

    // Automatically select the best implementation for your platform.
    // You can also access each implementation directly e.g. INotifyWatcher.
    let mut watcher: RecommendedWatcher =
        Watcher::new(tx, Duration::from_millis(200)).expect("Can't make watcher");

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    watcher
        .watch(head_dir(root.as_path()), RecursiveMode::Recursive)
        .expect("Can't watch");

    // This is a simple loop, but you may want to use more complex logic here,
    // for example to handle I/O.
    loop {
        match rx.recv() {
            Ok(event) => match event {
                notify::DebouncedEvent::Remove(_) => {
                    println!("Rebuilding...");
                    let now = std::time::Instant::now();
                    codebase.reload()?;
                    let hash = find_term(&mut codebase, term);
                    pack_term(&mut codebase, root.as_path(), &hash.0, outfile)?;
                    println!("Finished! {:?}", now.elapsed());
                }
                _ => (),
            },
            Err(e) => println!("watch error: {:?}", e),
        }
    }
}
