#![feature(exact_size_is_empty)]
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::Pass;
use std::env;
use std::env::Args;
use std::io::{Read, Write};
use std::fs::File;
use std::process::exit;

mod compiler;

struct Config {
    paths: Vec<String>,
    input: Option<String>,
    output: Option<String>,
}

fn parse_config(args: &mut Args) -> Config {
    let mut input = None;
    let mut output = None;
    let mut paths = vec![".".to_owned()];
    while !args.is_empty() {
        let arg = args.next().unwrap();
        match arg.as_str() {
            "-p" | "--path" => {
                paths.push(args.next().expect("Expected path"))
            },
            f if input.is_none() => input = Some(f.to_owned()),
            f if input.is_some() && output.is_none() => output = Some(f.to_owned()),
            s => panic!("Unexpected argument {}", s)
        }
    }
    Config {
        input,
        output,
        paths,
    }
}

fn main() {
    let mut args = env::args();
    args.next();
    let config = parse_config(&mut args);
    let mut buffer = String::new();
    let mut input: Box<dyn Read> = match &config.input {
        Some(f) => Box::new(File::open(f).unwrap()),
        None => Box::new(std::io::stdin()),
    };
    let mut output: Box<dyn Write> = match &config.output {
        Some(f) => Box::new(File::open(f).unwrap()),
        None => Box::new(std::io::stdout()),
    };
    input.read_to_string(&mut buffer).unwrap();

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
    let mut resolver = Resolver::new();
    let locals = match resolver.run(&ss) {
        Ok(l) => l,
        Err(es) => {
            es.iter().for_each(|e| eprintln!("{}", e));
            exit(1);
        },
    };
    output.write_all(format!("{:?}\n{:?}", ss, locals).as_bytes()).unwrap();
}
