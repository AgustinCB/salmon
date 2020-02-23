#![feature(exact_size_is_empty)]
#![feature(box_patterns)]

use crate::interpreter::Interpreter;
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::Pass;
use std::io::{self, Read};
use std::env;
use std::env::Args;
use std::process::exit;

mod class;
mod function;
mod interpreter;
mod state;
mod value;

struct Config {
    paths: Vec<String>,
}

fn parse_config(args: &mut Args) -> Config {
    let mut paths = vec![".".to_owned()];
    while !args.is_empty() {
        let arg = args.next().unwrap();
        match arg.as_str() {
            "-p" | "--path" => {
                paths.push(args.next().expect("Expected path"))
            },
            s => panic!("Unexpected argument {}", s)
        }
    }
    Config {
        paths,
    }
}

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();
    let config = parse_config(&mut args);
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer)?;

    let mut lexer = Lexer::new(buffer.as_str(), "stdin");
    let result = lexer
        .parse()
        .and_then(|ts| {
            let parser = Parser::new(ts.into_iter().peekable());
            parser.parse()
        });
    let ss = match result {
        Ok(ss) => ss,
        Err(es) => {
            es.iter().for_each(|e| eprintln!("{}", e));
            exit(1);
        },
    };
    let mut interpreter = Interpreter::new(&config.paths, "");
    let mut resolver = Resolver::new();
    let locals = match resolver.run(&ss) {
        Ok(l) => l,
        Err(es) => {
            es.iter().for_each(|e| eprintln!("{}", e));
            exit(1);
        },
    };
    interpreter.locals = locals;
    if let Err(e) = interpreter.run(&ss) {
        eprintln!("{}", e);
        exit(1);
    };
    Ok(())
}
