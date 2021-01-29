#![allow(dead_code)]

extern crate env_logger;
extern crate http;
extern crate notify;
extern crate serde_derive;
extern crate serde_json;

mod base32hex;
mod branch;
mod chicken;
mod env;
mod ffi;
mod ir;
mod pack;
mod parser;
mod printer;
mod run;
mod server;
mod unique;
mod visitor;

fn help() {
    println!(
        r#"Unison.rs - a wasm runtime and development environment for unison

Usage:
- unison.rs serve                     : run the development environment. view it at http://127.0.0.1:3030
- unison.rs serve path/to/codebase    : specify the codebase root (the default is ~/.unison/v1)
- unison.rs serve --override some/dir : provide an override directory for serving custom javascript files
"#
    );
}

fn main() -> std::io::Result<()> {
    env_logger::init();
    let mut args = std::env::args().collect::<Vec<String>>();
    args.remove(0);
    if args.len() == 0 {
        help();
        Ok(())
    } else {
        let cmd = args.remove(0);
        match (cmd.as_str(), args.as_slice()) {
            ("--help", []) => {
                help();
                Ok(())
            }
            ("test", [path]) => run::run_test(path),
            // ("runtime_tests", [path]) => run::runtime_tests(path),
            ("serve", []) => server::serve(crate::pack::default_root(), None),
            ("serve", [overridez, op]) if overridez == "--override" => {
                server::serve(crate::pack::default_root(), Some(op.to_owned()))
            }
            ("serve", [root]) => server::serve(std::path::PathBuf::from(root), None),
            // lol maybe I should get real arg parsing
            ("serve", [root, overridez, op]) if overridez == "--override" => {
                server::serve(std::path::PathBuf::from(root), Some(op.to_owned()))
            }
            ("serve", [overridez, op, root]) if overridez == "--override" => {
                server::serve(std::path::PathBuf::from(root), Some(op.to_owned()))
            }
            ("pack-watch", [path, output]) => pack::pack_watch(path, output),
            ("pack", [path, output]) => pack::pack(path, output),
            ("pack-chicken", [path, term, output]) => pack::pack_one_chicken(path, term, output),
            ("pack-all-chicken", [path, ns, output]) => {
                pack::pack_all_chicken(path, &[ns.clone()], output)
            }
            ("pack-json", [path, output]) => pack::pack_json(path, output),
            ("pack-all-json", [path, ns, output]) => {
                pack::pack_all_json(path, &[ns.clone()], output)
            }
            ("pack-all-json-watch", args) => pack::pack_all_json_watch(&args[1..], &args[0]),
            ("pack-all", [path, output]) => pack::pack_all(&std::path::PathBuf::from(path), output),
            // ("test-all", [path]) => run_all_tests(path),
            ("run", args) => run::run_cli_term(&args[0], &args[1..]),
            _ => {
                println!("Unknown invocation.");
                help();
                Ok(())
            }
        }
    }
}
