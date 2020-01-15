#![feature(exact_size_is_empty)]
#![feature(box_patterns)]

use crate::interpreter::Interpreter;
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::{ProgramError, Pass, Statement};
use std::io::{self, Read};
use parser::importer::Importer;
use std::cell::RefCell;
use std::env;
use std::rc::Rc;
use std::env::Args;

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

    let mut lexer = Lexer::new(buffer, "stdin".to_owned());
    let result = lexer
        .parse()
        .and_then(|ts| {
            let parser = Parser::new(ts.into_iter().peekable());
            parser.parse()
        })
        .and_then(|ss| {
            let interpreter = Rc::new(RefCell::new(Interpreter::new(ss)));
            let ss = interpreter.borrow().content().to_vec();
            run_passes(interpreter.clone(), &ss, &config)?;
            Ok(interpreter)
        })
        .and_then(|interpreter| interpreter.borrow_mut().run().map_err(|e| vec![e]));
    match result {
        Ok(_) => {}
        Err(es) => es.iter().for_each(|e| eprintln!("{}", e)),
    }
    Ok(())
}

fn run_passes(interpreter: Rc<RefCell<Interpreter>>, ss: &Vec<Statement>, config: &Config) -> Result<(), Vec<ProgramError>>{
    let mut resolver = Resolver::new(interpreter.clone());
    let mut importer = Importer::new(&config.paths, interpreter);
    let passes: Vec<&mut dyn Pass> = vec![&mut importer, &mut resolver];
    passes
        .into_iter()
        .map(|p| p.run(&ss))
        .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
    Ok(())
}
