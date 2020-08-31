extern crate env_logger;
extern crate serde_derive;

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

fn pack_term(terms_path: &std::path::Path, hash: &str, out: &str) -> std::io::Result<()> {
    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash));
    let runtime_env: shared::types::RuntimeEnv = ir_env.into();

    std::fs::write(out, shared::pack(&runtime_env, hash))?;

    Ok(())
}

fn run_term(
    terms_path: &std::path::Path,
    hash: &str,
) -> std::io::Result<std::rc::Rc<types::Value>> {
    let last = std::time::Instant::now();
    println!("Running {:?} - {}", terms_path, hash);
    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash));

    {
        let mut file = std::fs::File::create(format!("data/source-{}.txt", hash))?;

        file.write_all(b"[- ENV -]\n")?;
        for (k, v) in ir_env.terms.iter() {
            file.write_all(format!("] Value {:?}\n", k).as_bytes())?;
            for (n, i) in v.iter().enumerate() {
                file.write_all(format!("({}) {:?}\n", n, i).as_bytes())?;
            }
            file.write_all(b"\n")?;
            file.write_all(
                ir_env
                    .env
                    .raw_cache
                    .get(&k.to_string())
                    .unwrap()
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
    let ret = shared::ir_runtime::eval(runtime_env, hash, &mut trace);
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

fn main() -> std::io::Result<()> {
    env_logger::init();
    println!("Hello, world!");
    match std::env::args().collect::<Vec<String>>().as_slice() {
        [_, cmd, path] if cmd == "test" => run_test(path),
        [_, cmd, path, outpath] if cmd == "path" => pack(path, outpath),
        [_, file] => run(file),
        _ => {
            println!("Usage: process file.ub");
            Ok(())
        }
    }
}
