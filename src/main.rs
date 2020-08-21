use std::env;
extern crate env_logger;

mod base32hex;
mod parser;
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

fn load_full_branch(root: &std::path::Path) -> std::io::Result<types::RawBranch> {
    let root = std::path::PathBuf::from(root);
    let mut head = root.clone();
    head.push("_head");
    let entries = std::fs::read_dir(head.as_path())?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;
    // println!("Loading branch {}", )
    let name = entries[0].file_name().unwrap().to_str().unwrap().to_owned();
    let mut head = root.clone();
    head.push(name + ".ub");
    // println!("Yes: {:?}", head);
    let head = parser::Buffer::from_file(head.as_path())?.get_branch();
    resolve_branch(&root, head)
}

fn load_branch(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_branch();
    // println!("{:?}", _result);
    Ok(())
}

fn run_term(file: &std::path::Path) -> std::io::Result<()> {
    let mut env = runtime::Env::init(file.parent().unwrap().parent().unwrap());
    let hash_name = file.file_name().unwrap().to_str().unwrap();
    let res = env.load(hash_name.clone());
    use runtime::Eval;
    let ret = res.eval(
        &mut env,
        &runtime::Stack(vec![runtime::Frame::new(hash_name.to_owned())]),
    );
    // let result = parser::Buffer::from_file(file)?.get_term();
    println!("{:?}", res);
    println!("{:?}", ret);
    Ok(())
}

fn run(file: &String) -> std::io::Result<()> {
    let path = std::path::PathBuf::from(file);

    if path.ends_with("paths") {
        let base = path.parent().unwrap();

        let _full = load_full_branch(path.as_path())?;
        println!("Terms {:?}", _full.terms.d1.values());
        println!("Sub {:?}", _full.children.keys());
        println!("Terms: {}", _full.terms.d1.len());

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
        return run_term(path.as_path());
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
    match env::args().collect::<Vec<String>>().as_slice() {
        [_, file] => run(file),
        _ => {
            println!("Usage: process file.ub");
            Ok(())
        }
    }
}
