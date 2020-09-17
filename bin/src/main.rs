#![allow(dead_code)]

extern crate env_logger;
extern crate serde_derive;
extern crate serde_json;

mod base32hex;
mod branch;
mod env;
mod ffi;
mod ir;
mod pack;
mod parser;
mod printer;
mod run;
mod unique;
mod visitor;

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
            ("test", [path]) => run::run_test(path),
            ("pack", [path, output]) => pack::pack(path, output),
            ("pack-json", [path, output]) => pack::pack_json(path, output),
            ("pack-all-json", [path, output]) => pack::pack_all_json(path, output),
            ("pack-all", [path, output]) => pack::pack_all(&std::path::PathBuf::from(path), output),
            // ("test-all", [path]) => run_all_tests(path),
            ("run", args) => run::run_cli_term(&args[0], &args[1..]),
            _ => {
                println!("Unknown command");
                Ok(())
            }
        }
    }
}
