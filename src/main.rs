// use std::env;
extern crate env_logger;

mod base32hex;
mod env;
mod ir;
mod ir_runtime;
mod parser;
mod pattern;
mod runtime;
mod types;

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

fn run_term(terms_path: &std::path::Path, hash: &str) -> std::io::Result<()> {
    println!("Running {:?} - {}", terms_path, hash);
    let env = env::Env::init(terms_path.parent().unwrap());
    // let res = env.load(hash);
    // println!("{:?}", res);
    let mut ir_env = ir::GlobalEnv::new(env);
    ir_env.load(hash);
    // res.to_ir(&mut ir_env, &mut env);
    let ret = ir_runtime::eval(ir_env, hash);
    // use runtime::Eval;
    // let ret = res.eval(
    //     &mut env,
    //     &env::Stack(vec![env::Frame::new(hash.to_owned())]),
    // );
    // let result = parser::Buffer::from_file(file)?.get_term();
    // println!("{:?}", res);
    println!("-> {:?}", ret);
    Ok(())
}

fn run_term_(terms_path: &std::path::Path, hash: &str) -> std::io::Result<()> {
    println!("Running {:?} - {}", terms_path, hash);
    let mut env = env::Env::init(terms_path.parent().unwrap());
    // let hash_name = file.file_name().unwrap().to_str().unwrap();
    let res = env.load(hash);
    use runtime::Eval;
    let ret = res.eval(
        &mut env,
        &env::Stack(vec![env::Frame::new(hash.to_owned())]),
    );
    // let result = parser::Buffer::from_file(file)?.get_term();
    println!("{:?}", res);
    println!("-> {:?}", ret);
    Ok(())
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
            println!("{:?}", k);
            run_term_(&terms, &all_terms.get(&k).unwrap().to_string())?;
        }
    }

    Ok(())
}

fn run(file: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.ends_with("paths") {
        let base = path.parent().unwrap();

        // let root = std::path::PathBuf::from(root);
        let mut branch = types::Branch::load(&path, get_head(&path)?)?;
        // branch.load_children(&path, true)?;
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
        return run_term(
            path.parent().unwrap(),
            &path.file_name().unwrap().to_str().unwrap()[1..],
        );
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
