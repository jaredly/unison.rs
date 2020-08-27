extern crate env_logger;
extern crate serde_derive;

mod base32hex;
mod env;
mod ir;
mod ir_runtime;
mod parser;
mod pattern;
mod printer;
mod types;
mod unique;

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
    root: &std::path::Path,
    branch: types::Causal<types::RawBranch>,
) -> std::io::Result<types::RawBranch> {
    match branch {
        types::Causal::One(c) => Ok(c),
        types::Causal::Cons(h, mut b) => {
            let mut p = std::path::PathBuf::from(root);
            p.push(h.to_string() + ".ub");
            // println!("Resolve: {:?}", p);
            b.merge(&resolve_branch(
                root,
                parser::Buffer::from_file(p.as_path())?.get_branch(),
            )?);
            Ok(b)
        }
        types::Causal::Merge(hashes, mut b) => {
            for hash in hashes.iter() {
                let mut p = std::path::PathBuf::from(root);
                p.push(hash.to_string() + ".ub");
                b.merge(&resolve_branch(
                    root,
                    parser::Buffer::from_file(p.as_path())?.get_branch(),
                )?)
            }
            Ok(b)
        }
    }
}

impl types::Branch {
    fn load(root: &std::path::PathBuf, hash: String) -> std::io::Result<types::Branch> {
        let mut head = root.clone();
        head.push(hash + ".ub");
        // println!("Yes: {:?}", head);
        let head = parser::Buffer::from_file(head.as_path())?.get_branch();
        let head = resolve_branch(&root, head)?;
        Ok(types::Branch {
            raw: head,
            children: std::collections::HashMap::new(),
        })
    }

    fn load_children(&mut self, root: &std::path::PathBuf, deep: bool) -> std::io::Result<()> {
        for (k, v) in self.raw.children.iter() {
            let mut child = types::Branch::load(root, v.to_string())?;
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

// fn load_full_branch(root: &std::path::Path) -> std::io::Result<types::Branch> {
//     let root = std::path::PathBuf::from(root);
//     let mut branch = types::Branch::load(&root, get_head(&root)?)?;
//     branch.load_children(&root, true)?;
//     Ok(branch)
// }

fn load_branch(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_branch();
    Ok(())
}

fn run_term(terms_path: &std::path::Path, hash: &str) -> std::io::Result<types::Value> {
    // use tracing_chrome::ChromeLayerBuilder;
    // use tracing_subscriber::prelude::*;

    // let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    // tracing_subscriber::registry().with(chrome_layer).init();

    let last = std::time::Instant::now();
    println!("Running {:?} - {}", terms_path, hash);
    let env = env::Env::init(terms_path.parent().unwrap());
    // let res = env.load(hash);
    // println!("{:?}", res);
    let mut ir_env = ir::GlobalEnv::new(env);
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

    // res.to_ir(&mut ir_env, &mut env);
    // let guard = pprof::ProfilerGuard::new(100).unwrap();
    let mut trace = vec![];
    let ret = ir_runtime::eval(ir_env, hash, &mut trace);
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
    file.write_all(b"[")?;
    let mut lines = vec![];
    for trace in trace {
        lines.push(format!(
            r#"
            {{"cat": {:?}, "pid": 1, "ph": {:?}, "ts": {}, "name": {:?}, "tid": {}, "args": {{"[file]": {:?} }} }}
            "#,
            trace.cat,
            trace.ph,
            trace.ts.as_micros(),
            format!("{:?}", trace.name),
            trace.tid,
            trace.file
        ));
    }
    file.write_all(lines.join(",\n").as_bytes())?;
    file.write_all(b"]")?;
    // use runtime::Eval;
    // let ret = res.eval(
    //     &mut env,
    //     &env::Stack(vec![env::Frame::new(hash.to_owned())]),
    // );
    // let result = parser::Buffer::from_file(file)?.get_term();
    // println!("{:?}", res);
    // if let Ok(report) = guard.report().build() {
    //     let file = std::fs::File::create(format!("flamegraph-{}.svg", hash)).unwrap();
    //     report.flamegraph(file).unwrap();
    // };

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
    let mut branch = types::Branch::load(&paths, get_head(&paths)?)?;
    branch.load_children(&paths, true)?;

    let terms = path_with(&root, "terms");
    let mut all_terms = std::collections::HashMap::new();
    branch.collect_terms(&vec![], &mut all_terms);
    let mut keys: Vec<Vec<String>> = all_terms.keys().cloned().collect();
    keys.sort();
    for k in keys {
        if k[k.len() - 1] == "test" {
            println!("--> {:?}", k.join("."));
            if k.join(".") == "base.Int.inRange.test" {
                println!("Skipping");
                continue;
            }
            let hash = all_terms.get(&k).unwrap().to_string();
            let ret = run_term(&terms, &hash)?;
            use types::*;
            match ret {
                Value::Sequence(results) => {
                    for result in results {
                        match result {
                            Value::PartialConstructor(
                                Reference::DerivedId(Id(chash, _, _)),
                                num,
                                contents,
                            ) if chash.to_string().starts_with("vmc06s") => {
                                if num == 0 {
                                    // failed!
                                    println!("Test {} failed! {:?}", hash, contents);
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
            // println!("-> {:?}", ret);
            // run_term_(&terms, &all_terms.get(&k).unwrap().to_string())?;
        }
    }

    Ok(())
}

fn run(file: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.ends_with("paths") {
        // let base = path.parent().unwrap();
        let cache = std::path::PathBuf::from(".tests-cache");

        let branch = if cache.as_path().exists() {
            println!("Using cache");
            let mut buf = vec![];
            use std::io::Read;
            let mut file = std::fs::File::open(cache)?;
            file.read_to_end(&mut buf)?;
            bincode::deserialize(&buf).unwrap()
        } else {
            let mut branch = types::Branch::load(&path, get_head(&path)?)?;
            branch.load_children(&path, true)?;
            let buf = bincode::serialize(&branch).unwrap();
            use std::io::Write;
            let mut file = std::fs::File::create(cache)?;
            file.write_all(&buf)?;
            branch
        };
        // let root = std::path::PathBuf::from(root);

        // Ok(branch)

        // let _full = load_full_branch(path.as_path())?;
        println!("Terms {:?}", branch.raw.terms.d1.values());
        println!("Sub {:?}", branch.children.keys());
        println!("Terms: {}", branch.raw.terms.d1.len());

        // let entries = std::fs::read_dir(path)?
        //     .map(|res| res.map(|e| e.path()))
        //     .collect::<Result<Vec<_>, std::io::Error>>()?;

        // for mut entry in entries {
        //     println!("Checking folder: {:?}", entry);
        //     entry.push("compiled.ub");
        //     let branch = load_branch(entry.as_path())?;
        // }
        // println!("Parsed them all");
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
        );
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
        [_, file] => run(file),
        _ => {
            println!("Usage: process file.ub");
            Ok(())
        }
    }
}
