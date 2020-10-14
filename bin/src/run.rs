use crate::branch::Branch;
use crate::pack::{get_head, path_with};
use crate::parser;
use crate::{env, ffi, ir};
use shared::types;

#[derive(Debug)]
struct WrappedValue(String);

impl shared::convert::ConvertibleArg<WrappedValue> for WrappedValue {
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

pub fn run_cli_term(term: &String, args: &[String]) -> std::io::Result<()> {
    let project = crate::pack::default_root();

    let terms_path = {
        let mut path = project.clone();
        path.push("terms");
        path
    };
    // let hash_raw = &path.file_name().unwrap().to_str().unwrap()[1..];

    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);

    let root = terms_path.parent().unwrap();
    let paths = path_with(&root, "paths");
    let mut branch = Branch::load(&paths, get_head(&paths)?)?;
    // branch.load_children(&paths, true)?;
    // {
    //     let mut all_terms = std::collections::HashMap::new();
    //     branch.collect_terms(&vec![], &mut all_terms);

    //     let mut hashes: Vec<&Hash> = all_terms.values().collect();
    //     hashes.sort();
    //     for hash in hashes {
    //         let _ = ir_env.load(hash);
    //     }

    //     {
    //         let mut walker = TypeWalker(&mut ir_env.env);
    //         let ks: Vec<String> = walker.0.type_cache.keys().cloned().collect();
    //         for k in ks {
    //             use visitor::Accept;
    //             let mut m = walker.0.load_type(&k);
    //             m.accept(&mut walker);
    //         }
    //     }
    // };

    let hash = if &term[0..1] == "." {
        let hash = branch
            .find_term(
                &paths,
                &term[1..].split(".").collect::<Vec<&str>>().as_slice(),
            )
            .unwrap();
        ir_env.load(&hash).unwrap();
        hash
    } else {
        let hash = types::Hash::from_string(term);
        ir_env.load(&hash).unwrap();
        hash
    };

    // let hash = types::Hash::from_string(hash_raw);
    // ir_env.load(&hash).unwrap();

    let mut runtime_env: shared::types::RuntimeEnv = ir_env.into();

    let t = &runtime_env.terms.get(&hash).unwrap().1;
    println!("Type: {:?}", t);
    let (targs, effects, tres) = shared::ir_runtime::extract_args(t);

    let args = shared::convert::convert_args(
        args.into_iter().map(|x| WrappedValue(x.clone())).collect(),
        &targs,
    )
    .unwrap();
    println!("Got {:?} -- {:?} -- {:?}", targs, effects, tres);

    let run_hash = if args.len() > 0 {
        runtime_env.add_eval(&hash.to_string(), args).unwrap()
    } else {
        hash
    };

    let mut trace = shared::chrome_trace::Traces::new();

    let mut names = Default::default();
    branch.get_flat_names(&vec![], &mut names);
    let mut ffi = ffi::RustFFI(names, vec![]);

    // for effect in effects {
    //     use shared::ffi::FFI;
    //     if !ffi.handles(&effect) {
    //         return Err(std::io::ErrorKind::InvalidInput.into());
    //     }
    // }

    let mut state = shared::state::State::new_value(
        &runtime_env,
        run_hash,
        false,
        shared::state::build_effects_map(effects),
    );
    println!("[---running---]");
    let ret = state
        .run_to_end(&mut ffi, &mut trace)
        .expect("Invalid FFI I guess");
    match ret {
        None => (),
        Some(ret) => println!("-> {}", crate::printer::value_to_pretty(&ret, &ffi.0, 100)),
    };

    while ffi.has_next_request() {
        println!("> Async handler");
        ffi.process_next_request(&runtime_env, &mut trace)
            .expect("Invalid FFI Response");
    }
    // let ret = shared::ir_runtime::eval(&runtime_env, eval_hash, &mut trace);

    // std::fs::File::create("trace.json");
    // std::fs::write(
    //     "trace.json",
    //     serde_json::to_string(&state.stack.traces.0[0..200.min(state.stack.traces.0.len())])
    //         .unwrap(),
    // )?;

    Ok(())
}

pub fn run_term(
    terms_path: &std::path::Path,
    hash: &str,
) -> std::io::Result<std::sync::Arc<types::Value>> {
    let last = std::time::Instant::now();
    println!("Running {:?} - {}", terms_path, hash);
    let env = env::Env::init(terms_path.parent().unwrap());
    let mut ir_env = ir::TranslationEnv::new(env);
    ir_env.load(&types::Hash::from_string(hash)).unwrap();
    use crate::printer::ToPretty;

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
                    // TODO maybe do names here? although having the hashes is maybe most helpful in this logging case
                    .to_pretty(80, &Default::default())
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

    let mut trace = shared::chrome_trace::Traces::new();
    let names = Default::default();
    // branch.get_flat_names(&vec![], &mut names);
    let mut ffi = ffi::RustFFI(names, vec![]);
    let ret = shared::ir_runtime::eval(
        &runtime_env,
        &mut ffi,
        hash,
        &mut trace,
        true,
        Default::default(),
    )
    .expect("Invalid FFI")
    .unwrap();
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

// pub fn runtime_tests(root: &str) -> std::io::Result<()> {
//     let root = std::path::PathBuf::from(root);
//     println!("Running all tests I can find");
//     // let paths = path_with(&root, "paths");

//     let mut all_terms = std::collections::HashMap::new();

//     let mut codebase = crate::branch::Codebase::new(root.to_owned())?;
//     codebase.load_all()?;

//     let mut branch = Branch::load(&paths, get_head(&paths)?)?;
//     branch.load_children(&paths, true)?;
// }

pub fn run_test(root: &str) -> std::io::Result<()> {
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

fn load_branch(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_branch();
    Ok(())
}

pub fn run(file: &String) -> std::io::Result<()> {
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
