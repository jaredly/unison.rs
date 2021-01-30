use crate::branch::Codebase;
use crate::env;
use crate::ir;
use crate::visitor;
use serde_derive::Serialize;
use shared::types;
use shared::types::*;
use std::collections::{HashMap, HashSet};

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
            Term::Constructor(Reference::DerivedId(id), _) => {
                let id = id.to_string();
                if !self.0.has_type(&id) {
                    use visitor::Accept;
                    self.0.load_type(&id).accept(self);
                }
            }
            _ => (),
        };
        true
    }

    fn visit_type(&mut self, typ: &mut Type) -> bool {
        match typ {
            Type::Ref(Reference::DerivedId(id)) => {
                let id = id.to_string();
                if !self.0.has_type(&id) {
                    use visitor::Accept;
                    self.0.load_type(&id).accept(self);
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

    let mut all_terms = HashMap::new();
    codebase.collect_terms(&codebase.head.clone(), &vec![], &mut all_terms);

    let env = env::Env::init(root);
    let mut ir_env = ir::TranslationEnv::new(env);

    let mut hashes: Vec<&Id> = all_terms.values().collect();
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

impl Default for Names<Id> {
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

// fn branch_names(codebase: &Codebase) -> Names<Id> {
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

pub fn env_names(names: &Names<Id>, runtime_env: &RuntimeEnv) -> Names<String> {
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

pub fn terms_to_env(root: &std::path::Path, hashes: Vec<Id>) -> std::io::Result<types::RuntimeEnv> {
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
    ir_env.load(&types::Id::from_string(hash)).unwrap();

    walk_env(&mut ir_env.env);

    Ok(ir_env.into())
}

#[derive(Serialize, Debug)]
struct JsonEnv {
    terms: HashMap<String, (Vec<IR>, ABT<Type>)>,
    types: HashMap<String, TypeDecl>,
    anon_fns: Vec<(Id, Vec<IR>)>,
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

pub fn pack_term(codebase: &mut Codebase, hash: &str, out: &str) -> std::io::Result<()> {
    let runtime_env = term_to_env(codebase.root().as_path(), hash)?;

    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string_pretty(&env_names(&codebase.get_names(), &runtime_env).serialize())
            .unwrap(),
    )?;

    std::fs::write(out, shared::pack(&runtime_env))?;

    Ok(())
}

// ok, so real talk solution would be:
// I mean if I still wanted a near total ordering...

fn topo_visit(
    key: &Hash,
    deps: &mut HashMap<Hash, HashSet<Hash>>,
    res: &mut Vec<Hash>,
    visiting: &mut HashSet<Hash>,
) {
    if !deps.contains_key(key) {
        return;
    }
    // oof. hm ok.
    // should I limit this to things that need to be immediately available?
    // that way I can guarantee no loops.
    if visiting.contains(key) {
        // unreachable!("Loop");
        return; // it's fine
    }
    visiting.insert(key.clone());
    let v = deps.get(key).unwrap().clone();
    let mut v = v.iter().collect::<Vec<&Hash>>();
    v.sort();
    for k in v {
        if k == key {
            continue;
        }
        topo_visit(&k, deps, res, visiting);
    }
    deps.remove(key);
    visiting.remove(key);
    res.insert(0, key.clone());
}

fn topo_sort(mut deps: HashMap<Hash, HashSet<Hash>>) -> Vec<Hash> {
    let mut visiting = HashSet::new();
    let mut res = vec![];
    loop {
        let key = {
            let mut keys = deps.keys().cloned().collect::<Vec<Hash>>();
            keys.sort();
            match keys.get(0) {
                None => break,
                Some(key) => key.clone(),
            }
        };
        topo_visit(&key, &mut deps, &mut res, &mut visiting);
        // let v = deps.get(key).unwrap();
    }
    res.reverse();
    return res;
}

fn sort_circular(ids: &mut Vec<Id>, deps: &HashMap<Id, HashMap<Id, bool>>) {
    ids.sort_by(
        |a, b| match (deps.get(a).unwrap().get(b), deps.get(b).unwrap().get(a)) {
            (Some(true), Some(true)) => unreachable!("Immediate loop, should not be possible"),
            (_, Some(true)) => std::cmp::Ordering::Less,
            (Some(true), _) => std::cmp::Ordering::Greater,
            (_, _) => std::cmp::Ordering::Equal,
        },
    )
}

fn topo_sort_ids(deps: HashMap<Id, HashMap<Id, bool>>) -> Vec<Id> {
    let mut hash_deps = HashMap::new();
    let mut hash_to_ids = HashMap::new();
    for (k, v) in &deps {
        if !hash_deps.contains_key(&k.hash) {
            hash_deps.insert(k.hash.clone(), HashSet::new());
            hash_to_ids.insert(k.hash.clone(), vec![]);
        }
        hash_to_ids.get_mut(&k.hash).unwrap().push(k.clone());
        hash_deps
            .get_mut(&k.hash)
            .unwrap()
            .extend(v.iter().map(|h| h.0.hash.clone()));
    }
    let sorted_hashes = topo_sort(hash_deps);
    let mut res = vec![];
    for hash in &sorted_hashes {
        match hash_to_ids.remove(hash) {
            None => (),
            Some(mut ids) => {
                if ids.len() > 1 {
                    sort_circular(&mut ids, &deps);
                }
                res.extend(ids.into_iter());
            }
        }
    }
    // STOPSHIP HERE
    return res;
}

// fn term_to_chichen_env(
//     root: &std::path::Path,
//     hash: &str,
// ) ->

fn pack_chicken_env(
    mut chicken_env: crate::chicken::TranslationEnv,
    all_types: HashMap<Id, Vec<Vec<String>>>,
    all_terms: HashMap<Vec<String>, Id>,
) -> std::io::Result<String> {
    let mut output = vec!["(load \"stdlib.scm\")\n".to_owned()];

    // the types folks
    for (hash, decl) in chicken_env.types.iter() {
        all_types.get(hash).map(|names| {
            output.push(format!("; {}", names[0].join(".")));
        });
        output.push(format!("; {:?}", decl));
        match decl {
            TypeDecl::Effect(DataDecl { constructors, .. }) => {
                for (i, (name, t)) in constructors.iter().enumerate() {
                    output.push(format!("; {}", name.to_atom()));
                    let name = format!("{}_{}", hash.to_string(), i);
                    output.push(super::chicken::ability_to_chicken(&name, t).to_string());
                }
            }
            TypeDecl::Data(DataDecl { constructors, .. }) => {
                for (i, (name, t)) in constructors.iter().enumerate() {
                    output.push(format!("; {}", name.to_atom()));
                    let name = format!("{}_{}", hash.to_string(), i);
                    output.push(super::chicken::ability_to_type(&name, t).to_string());
                }
            }
        }
        // deps.insert(hash.clone(), HashSet::new());
    }

    let mut names_for_terms: HashMap<Id, Vec<Vec<String>>> = HashMap::new();
    for (k, v) in all_terms.iter() {
        let mut current = names_for_terms.get(v).cloned().unwrap_or_default();
        current.push(k.to_owned());
        names_for_terms.insert(v.clone(), current);
    }

    let mut deps = HashMap::new();
    for (k, (_, _, kdeps)) in &chicken_env.terms {
        deps.insert(k.to_owned(), kdeps.to_owned());
    }

    let sorted = topo_sort_ids(deps);
    for hash in &sorted {
        let typ = chicken_env.env.load(&hash.to_string()).map(|m| m.1).ok();
        let names = names_for_terms.get(hash);
        names.map(|names| {
            println!("Packing {}", names[0].join("."));
            output.push(format!("; {} : {:?}", names[0].join("."), typ));
            output.push(format!("(print-processing {:?})", names[0].join(".")));
        });
        output.push(chicken_env.to_string(hash));

        names.map(|names| {
            let first = &names[0];
            output.push(format!("; /end {}", first.join(".")));

            match typ {
                Some(ABT::Tm(Type::Ref(Reference::Builtin(b)))) if b == "Boolean" => {
                    if first[first.len() - 1].starts_with("t_") {
                        output.push(format!(
                            "(check {} {:?})",
                            hash.to_string(),
                            first.join(".")
                        ))
                    }
                }
                Some(ABT::Tm(Type::App(one, two))) => match (&*one, &*two) {
                    (
                        ABT::Tm(Type::Ref(Reference::Builtin(b))),
                        ABT::Tm(Type::Ref(Reference::DerivedId(tid)))
                    ) if b == "Sequence" &&
                    &tid.to_string() == "vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0" => {
                        output.push(format!(
                            "(check-results {} {:?})",
                            hash.to_string(),
                            first.join(".")
                        ))

                    }
                    _ => ()
                }
                _ => ()
            }

        });
    }

    Ok(output.join("\n\n"))
}

fn pack_one_chicken_inner(
    codebase: &mut Codebase,
    outfile: &String,
    term: &String,
) -> std::io::Result<()> {
    let hash = find_term(codebase, term);
    println!("Mapped term name {} to hash {:?}", term, hash);

    let mut all_terms = HashMap::new();
    let mut all_types = HashMap::new();

    // TODO: maybe just the owning namespace of the term?
    // Or allow you to specify? It's just a perf thing after all...
    let ns = codebase.head.clone();
    codebase.collect_terms(&ns, &vec![], &mut all_terms);
    codebase.collect_types(&ns, &vec![], &mut all_types);

    let env = env::Env::init(codebase.root().as_path());
    use crate::chicken::TranslationEnv;
    let mut chicken_env = TranslationEnv::new(env);

    match chicken_env.load(&hash) {
        Err(env::Error::TermNotFound(term)) => {
            println!("Cant load a thin");
            unreachable!("Unable to find term {}", term);
        }
        _ => (),
    }

    std::fs::write(
        outfile,
        pack_chicken_env(chicken_env, all_types, all_terms)?,
    )?;

    Ok(())
}

fn pack_all_chicken_inner(
    codebase: &mut Codebase,
    outfile: &String,
    ns: &[String],
) -> std::io::Result<()> {
    let mut all_terms = HashMap::new();
    let mut all_types = HashMap::new();

    for ns in ns {
        let ns = if ns == &"" || ns == &"." {
            codebase.head.clone()
        } else {
            codebase
                .find_ns(ns.split(".").collect::<Vec<&str>>().as_slice())
                .expect("Namespace not found in codebase.")
                .to_string()
        };

        codebase.collect_terms(&ns, &vec![], &mut all_terms);
        codebase.collect_types(&ns, &vec![], &mut all_types);
    }

    let env = env::Env::init(codebase.root().as_path());
    use crate::chicken::TranslationEnv;
    let mut chicken_env = TranslationEnv::new(env);
    // let mut ir_env = ir::TranslationEnv::new(env);

    let mut named_terms = HashSet::new();
    for v in all_terms.values() {
        named_terms.insert(v);
    }

    let hashes: Vec<&Id> = all_terms.values().collect();
    for hash in &hashes {
        // don't need it, thanks
        if !named_terms.contains(hash) {
            continue;
        }
        match chicken_env.load(*hash) {
            Err(env::Error::TermNotFound(term)) => println!(
                "Unable to pack {:?} - depended on missing term {:?}",
                hash, term
            ),
            Err(err) => {
                unreachable!("{:?}", err)
            }
            Ok(_) => {
                // deps.insert((*hash).to_owned(), uses);
            }
        }
    }

    std::fs::write(
        outfile,
        pack_chicken_env(chicken_env, all_types, all_terms)?,
    )?;

    Ok(())
}

fn pack_all_json_inner(
    codebase: &mut Codebase,
    outfile: &String,
    ns: &[String],
) -> std::io::Result<()> {
    let mut all_terms = HashMap::new();

    for ns in ns {
        let ns = if ns == &"" || ns == &"." {
            codebase.head.clone()
        } else {
            codebase
                .find_ns(ns.split(".").collect::<Vec<&str>>().as_slice())
                .unwrap()
                .to_string()
        };

        codebase.collect_terms(&ns, &vec![], &mut all_terms);
    }

    let env = env::Env::init(codebase.root().as_path());
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

pub fn pack_one_chicken(file: &String, term: &String, outfile: &String) -> std::io::Result<()> {
    let terms_path = std::path::PathBuf::from(file);

    println!("Packing the terms");
    let root = terms_path.parent().unwrap();
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;

    pack_one_chicken_inner(&mut codebase, outfile, term)
}

pub fn pack_all_chicken(file: &String, ns: &[String], outfile: &String) -> std::io::Result<()> {
    let terms_path = std::path::PathBuf::from(file);

    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;

    pack_all_chicken_inner(&mut codebase, outfile, ns)
}

pub fn pack_all_json(file: &String, ns: &[String], outfile: &String) -> std::io::Result<()> {
    let terms_path = std::path::PathBuf::from(file);

    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let mut codebase = Codebase::new(root.to_owned())?;
    codebase.load_all()?;

    pack_all_json_inner(&mut codebase, outfile, ns)
}

pub fn pack_all_json_watch(ns: &[String], outfile: &String) -> std::io::Result<()> {
    println!("Packing all the terms I can find");
    watch(move |codebase| {
        codebase.load_all()?;
        pack_all_json_inner(codebase, outfile, ns)
    })
}

pub fn default_root() -> std::path::PathBuf {
    let mut project: std::path::PathBuf = std::env::var("HOME").unwrap().into();
    project.push(".unison");
    project.push("v1");
    project
}

pub fn find_term(codebase: &mut Codebase, term: &str) -> Id {
    let term = if &term[0..1] == "." { &term[1..] } else { term };
    // if &term[0..1] == "." {
    codebase
        .find_term(term.split(".").collect::<Vec<&str>>().as_slice())
        .unwrap()
    // } else {
    //     types::Id::from_string(term)
    // }
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
        pack_term_json(codebase, root.as_path(), &hash.to_string(), outfile)?;
    }

    return Ok(());
}

pub fn pack(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.exists() {
        let root = path.parent().unwrap().parent().unwrap();
        let mut codebase = load_main_branch(root)?;
        let hash = &path.file_name().unwrap().to_str().unwrap()[1..];
        pack_term(&mut codebase, &hash, outfile)
    } else {
        let root = default_root();
        let mut codebase = load_main_branch(root.as_path())?;
        let hash = find_term(&mut codebase, file);
        pack_term(&mut codebase, &hash.to_string(), outfile)
    }
}

pub fn pack_watch(term: &String, outfile: &String) -> std::io::Result<()> {
    watch(move |codebase| {
        let hash = find_term(codebase, term);
        pack_term(codebase, &hash.to_string(), outfile)
    })
}

fn watch<F: Fn(&mut Codebase) -> std::io::Result<()>>(run: F) -> std::io::Result<()> {
    use notify::{RecommendedWatcher, RecursiveMode, Watcher};
    use std::sync::mpsc::channel;
    use std::time::Duration;

    let root = default_root();
    // TODO incremental reload!!!

    let now = std::time::Instant::now();
    println!("Initial build...");
    let mut codebase = load_main_branch(root.as_path())?;
    run(&mut codebase)?;
    println!("Finished! {:?}", now.elapsed());

    // Create a channel to receive the events.
    let (tx, rx) = channel();
    let mut watcher: RecommendedWatcher =
        Watcher::new(tx, Duration::from_millis(200)).expect("Can't make watcher");
    watcher
        .watch(head_dir(root.as_path()), RecursiveMode::Recursive)
        .expect("Can't watch");

    loop {
        match rx.recv() {
            Ok(event) => match event {
                notify::DebouncedEvent::Remove(_) => {
                    println!("Rebuilding...");
                    let now = std::time::Instant::now();
                    codebase.reload()?;
                    run(&mut codebase)?;
                    println!("Finished! {:?}", now.elapsed());
                }
                _ => (),
            },
            Err(e) => println!("watch error: {:?}", e),
        }
    }
}
