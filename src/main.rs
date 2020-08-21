use std::env;

mod base32hex;
mod parser;

fn process_file(file: &std::path::Path) -> std::io::Result<()> {
    if !file.is_file() {
        return Ok(());
    }
    let _result = parser::Buffer::from_file(file)?.get_term();
    // println!("{:?}", result);
    Ok(())
}

fn run(file: &String) -> std::io::Result<()> {
    if file.ends_with(".ub") {
        let path = std::path::PathBuf::from(file);
        return process_file(path.as_path());
    }
    let entries = std::fs::read_dir(file)?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, std::io::Error>>()?;

    for mut entry in entries {
        println!("Checking folder: {:?}", entry);
        entry.push("compiled.ub");
        process_file(entry.as_path())?
    }
    println!("Parsed them all");
    Ok(())
}

fn main() -> std::io::Result<()> {
    println!("Hello, world!");
    match env::args().collect::<Vec<String>>().as_slice() {
        [_, file] => run(file),
        _ => {
            println!("Usage: process file.ub");
            Ok(())
        }
    }
}
