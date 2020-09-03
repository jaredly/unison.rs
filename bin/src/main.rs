extern crate env_logger;
extern crate serde_derive;
extern crate serde_json;

mod env;
mod ir;
mod parser;
mod printer;
mod unique;
mod visitor;
use shared::types;

fn load_type(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_type();
    println!("{:?}", _result);
    Ok(())
}

fn load_term(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_term();
    println!("{:?}", _result);
    Ok(())
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

use printer::ToPretty;
use shared::types::*;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub enum Causal<Contents> {
    One(Contents),
    Cons(Hash, Contents),
    Merge(Vec<Hash>, Contents),
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
    fn load(root: &std::path::PathBuf, hash: String) -> std::io::Result<Branch> {
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

    fn load_children(&mut self, root: &std::path::PathBuf, deep: bool) -> std::io::Result<()> {
        for (k, v) in self.raw.children.iter() {
            let mut child = Branch::load(root, v.to_string())?;
            if deep {
                child.load_children(root, deep)?;
            }
            self.children.insert(k.clone(), child);
        }
        Ok(())
    }

    fn type_names(
        &self,
        path: &Vec<String>,
        names: &mut std::collections::HashMap<types::Hash, Vec<Vec<String>>>,
    ) {
        for (k, v) in self.raw.types.d1.iter() {
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

    fn collect_names(
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

    fn collect_terms(
        &self,
        path: &Vec<String>,
        dest: &mut std::collections::HashMap<Vec<String>, types::Hash>,
    ) {
        for (k, v) in self.raw.terms.d1.iter() {
            match k {
                types::Referent::Ref(types::Reference::DerivedId(types::Id(hash, _, _))) => {
                    let mut full = path.clone();
                    full.push(v.text.clone());
                    dest.insert(full, hash.clone());
                }
                _ => (),
            }
        }
        for (k, v) in &self.children {
            let mut full = path.clone();
            full.push(k.text.clone());
            v.collect_terms(&full, dest);
        }
    }
}

fn get_head(root: &std::path::Path) -> std::io::Result<String> {
    let root = std::path::PathBuf::from(root);
    let mut head = root.clone();
    head.push("_head");
    let entries = std::fs::read_dir(head.as_path())?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
    let name = entries[0].file_name().unwrap().to_str().unwrap().to_owned();
    Ok(name)
}

fn load_branch(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_branch();
    Ok(())
}

struct TypeWalker<'a>(&'a mut crate::env::Env);

impl<'a> visitor::Visitor for TypeWalker<'a> {
    fn visit_abt<Inner: visitor::Accept>(&mut self, _: &mut ABT<Inner>) -> bool {
        true
    }
    fn visit_term(&mut self, _: &mut Term) -> bool {
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

fn pack_all(terms_path: &std::path::Path, out: &str) -> std::io::Result<()> {
    println!("Packing all the terms I can find");
    let root = terms_path.parent().unwrap();
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    let mut all_terms = std::collections::HashMap::new();
    branch.collect_terms(&vec![], &mut all_terms);

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

    std::fs::write(out, shared::pack(&runtime_env))?;
    std::fs::write(
        out.to_owned() + ".json",
        serde_json::to_string(&env_names(&branch, &runtime_env)).unwrap(),
    )?;

    Ok(())
}

fn env_names(
    branch: &Branch,
    runtime_env: &RuntimeEnv,
) -> (
    HashMap<String, Vec<Vec<String>>>,
    HashMap<String, HashMap<usize, Vec<Vec<String>>>>,
    HashMap<String, Vec<Vec<String>>>,
) {
    let mut all_names = HashMap::new();
    let mut all_constr_names = HashMap::new();
    branch.collect_names(&vec![], &mut all_names, &mut all_constr_names);
    let mut all_type_names = HashMap::new();
    branch.type_names(&vec![], &mut all_type_names);

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

fn pack_term(terms_path: &std::path::Path, hash: &str, out: &str) -> std::io::Result<()> {
    let root = terms_path.parent().unwrap();
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

    let runtime_env: shared::types::RuntimeEnv = ir_env.into();

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

fn run_term(
    terms_path: &std::path::Path,
    hash: &str,
) -> std::io::Result<std::sync::Arc<types::Value>> {
    let last = std::time::Instant::now();
    println!("Running {:?} - {}", terms_path, hash);
    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash)).unwrap();

    {
        let mut file = std::fs::File::create(format!("data/source-{}.txt", hash))?;

        file.write_all(b"[- ENV -]\n")?;
        for (k, (v, t)) in ir_env.terms.iter() {
            file.write_all(format!("] Value {:?}\n", k).as_bytes())?;
            file.write_all(format!("Type signature: {:?}\n", t).as_bytes())?;
            for (n, i) in v.iter().enumerate() {
                file.write_all(format!("({}) {:?}\n", n, i).as_bytes())?;
            }
            file.write_all(b"\n")?;
            file.write_all(
                ir_env
                    .env
                    .term_cache
                    .get(&k.to_string())
                    .unwrap()
                    .0
                    .to_pretty(80)
                    .as_bytes(),
            )?;
            file.write_all(b"\n\n")?;
        }
        for (i, v) in ir_env.anon_fns.iter().enumerate() {
            file.write_all(format!("] Fn({}) : {:?}\n", i, v.0).as_bytes())?;
            for (n, i) in v.1.iter().enumerate() {
                file.write_all(format!("({}) {:?}\n", n, i).as_bytes())?;
            }
            file.write_all(b"\n\n")?;
        }
    };

    let runtime_env: shared::types::RuntimeEnv = ir_env.into();

    let mut trace = shared::trace::Traces::new();
    let ret = shared::ir_runtime::eval(&runtime_env, hash, &mut trace);
    println!(
        "Time: {}ms ({}ns)",
        last.elapsed().as_millis(),
        last.elapsed().as_nanos()
    );

    let mut file = std::fs::File::create({
        let mut i = 0;
        let mut name;
        loop {
            name = if i == 0 {
                format!("data/profile-{}.json", hash)
            } else {
                format!("data/profile-{}-{}.json", hash, i)
            };
            if std::path::Path::new(&name).exists() {
                i += 1;
            } else {
                break;
            }
        }
        name
    })?;
    use std::io::Write;
    trace.to_file(&mut file)?;

    Ok(ret)
}

fn path_with(path: &std::path::Path, part: &str) -> std::path::PathBuf {
    let mut m = std::path::PathBuf::from(path);
    m.push(part);
    m
}

fn run_test(root: &str) -> std::io::Result<()> {
    let root = std::path::PathBuf::from(root);
    println!("Running all tests I can find");
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    let terms = path_with(&root, "terms");
    let mut all_terms = std::collections::HashMap::new();
    branch.collect_terms(&vec![], &mut all_terms);
    let mut keys: Vec<Vec<String>> = all_terms.keys().cloned().collect();
    keys.sort();
    for k in keys {
        if k[k.len() - 1] == "test" {
            if k.iter().position(|x| x.starts_with("_")) != None {
                continue; // hidden, ignore
            }
            println!("--> {:?}", k.join("."));
            let hash = all_terms.get(&k).unwrap().to_string();
            let ret = run_term(&terms, &hash)?;
            use types::*;
            match &*ret {
                Value::Sequence(results) => {
                    for result in results {
                        match &**result {
                            Value::PartialConstructor(
                                Reference::DerivedId(Id(chash, _, _)),
                                num,
                                contents,
                            ) if chash.to_string().starts_with("vmc06s") => {
                                if *num == 0 {
                                    // failed!
                                    println!(
                                        "Test {} failed! {:?}",
                                        hash,
                                        contents
                                            .iter()
                                            .map(|m| (&**m))
                                            .collect::<Vec<&types::Value>>()
                                    );
                                    return Ok(());
                                } else {
                                    println!(". Test result passed");
                                }
                            }
                            item => println!("Sequence item, not a `Result`: {:?}", item),
                        }
                    }
                }
                ret => println!("Test result, not a sequence: {:?}", ret),
            }
            println!("<-- all passed");
        }
    }

    Ok(())
}

fn pack(file: &String, outfile: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    pack_term(
        path.parent().unwrap(),
        &path.file_name().unwrap().to_str().unwrap()[1..],
        outfile,
    )?;
    return Ok(());
}

fn run(file: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.ends_with("paths") {
        // let cache = std::path::PathBuf::from(".tests-cache");

        // let branch = if cache.as_path().exists() {
        //     println!("Using cache");
        //     let mut buf = vec![];
        //     use std::io::Read;
        //     let mut file = std::fs::File::open(cache)?;
        //     file.read_to_end(&mut buf)?;
        //     bincode::deserialize(&buf).unwrap()
        // } else {
        //     let mut branch = Branch::load(&path, get_head(&path)?)?;
        //     branch.load_children(&path, true)?;
        //     let buf = bincode::serialize(&branch).unwrap();
        //     use std::io::Write;
        //     let mut file = std::fs::File::create(cache)?;
        //     file.write_all(&buf)?;
        //     branch
        // };
        let mut branch = Branch::load(&path, get_head(&path)?)?;
        branch.load_children(&path, true)?;
        // let buf = bincode::serialize(&branch).unwrap();
        // use std::io::Write;
        // let mut file = std::fs::File::create(cache)?;
        // file.write_all(&buf)?;

        println!("Terms {:?}", branch.raw.terms.d1.values());
        println!("Sub {:?}", branch.children.keys());
        println!("Terms: {}", branch.raw.terms.d1.len());

        return Ok(());
    }

    if path.ends_with("types") {
        let entries = std::fs::read_dir(path)?
            .map(|res| res.map(|e| e.path()))
            .collect::<Result<Vec<_>, std::io::Error>>()?;

        for mut entry in entries {
            println!("Checking folder: {:?}", entry);
            entry.push("compiled.ub");
            load_type(entry.as_path())?;
        }
        println!("Parsed them all");
        return Ok(());
    }

    if path.parent().unwrap().ends_with("terms") {
        let ret = run_term(
            path.parent().unwrap(),
            &path.file_name().unwrap().to_str().unwrap()[1..],
        )?;
        println!("-> {:?}", ret);
        return Ok(());
    }

    if path.parent().unwrap().parent().unwrap().ends_with("types") {
        return load_type(path.as_path());
    }
    if file.ends_with("compiled.ub") {
        return load_term(path.as_path());
    }
    if path.parent().unwrap().ends_with("paths") {
        return load_branch(path.as_path());
    }
    if file.ends_with(".ub") {
        return load_term(path.as_path());
    }
    let entries = std::fs::read_dir(path)?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;

    for mut entry in entries {
        println!("Checking folder: {:?}", entry);
        entry.push("compiled.ub");
        load_term(entry.as_path())?
    }
    println!("Parsed them all");
    Ok(())
}

#[derive(Debug)]
struct WrappedValue(String);

impl shared::ir_runtime::ConvertibleArg<WrappedValue> for WrappedValue {
    fn as_f64(&self) -> Option<f64> {
        self.0.parse().ok()
    }
    fn as_string(&self) -> Option<String> {
        self.0.parse().ok()
    }
    fn as_list(&self) -> Option<Vec<Self>> {
        None
    }
    fn is_empty(&self) -> bool {
        self.0.len() == 0
    }
}

fn run_cli_term(file: &String, args: &[String]) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    let terms_path = path.parent().unwrap();
    let hash_raw = &path.file_name().unwrap().to_str().unwrap()[1..];

    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);
    let hash = types::Hash::from_string(hash_raw);
    ir_env.load(&hash).unwrap();

    let mut runtime_env: shared::types::RuntimeEnv = ir_env.into();

    let t = &runtime_env.terms.get(&hash).unwrap().1;
    println!("Type: {:?}", t);
    let (targs, effects, tres) = shared::ir_runtime::extract_args(t);
    let args = shared::ir_runtime::convert_args(
        args.into_iter().map(|x| WrappedValue(x.clone())).collect(),
        &targs,
    )
    .unwrap();
    println!("Got {:?} -- {:?} -- {:?}", targs, effects, tres);

    let eval_hash = runtime_env.add_eval(hash_raw, args).unwrap();

    let mut trace = shared::trace::Traces::new();

    let mut state = shared::ir_runtime::State::new_value(&runtime_env, eval_hash);
    let ret = state.run_to_end(&mut trace);
    // let ret = shared::ir_runtime::eval(&runtime_env, eval_hash, &mut trace);

    println!("-> {:?}", ret);

    Ok(())
}

fn main() -> std::io::Result<()> {
    env_logger::init();
    println!("Hello, world!");
    let mut args = std::env::args().collect::<Vec<String>>();
    args.remove(0);
    if args.len() == 0 {
        println!("Commands: test, pack, pack-all, run, test-all");
        Ok(())
    } else {
        let cmd = args.remove(0);
        match (cmd.as_str(), args.as_slice()) {
            ("test", [path]) => run_test(path),
            ("pack", [path, output]) => pack(path, output),
            ("pack-all", [path, output]) => pack_all(&std::path::PathBuf::from(path), output),
            // ("test-all", [path]) => run_all_tests(path),
            ("run", args) => run_cli_term(&args[0], &args[1..]),
            _ => {
                println!("Unknown command");
                Ok(())
            }
        }
    }
    // match args.as_slice() {
    //     [_, cmd, path] if cmd == "test" => run_test(path),
    //     [_, cmd, path, outpath] if cmd == "pack" => pack(path, outpath),
    //     [_, cmd, path, outpath] if cmd == "pack-all" => {
    //         pack_all(&std::path::PathBuf::from(path), outpath)
    //     }
    //     [_, cmd, path, ...rest] if cmd == "run" => pack(path, outpath),
    //     [_, file] => run(file),
    //     _ => {
    //         println!("Usage: process file.ub");
    //         Ok(())
    //     }
    // }
}
