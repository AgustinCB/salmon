#![feature(exact_size_is_empty)]
use failure::Error;
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::{Pass, ProgramError, Literal, DataKeyword, SourceCodeLocation};
use std::env;
use std::env::Args;
use std::io::{Read, Write};
use std::fs::File;
use std::process::exit;
use smoked::cpu::{Value, Location};
use smoked::instruction::Instruction;
use smoked::serde::to_bytes;
use std::collections::{HashMap, HashSet};

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

fn handle_result<T>(result: Result<T, Vec<ProgramError>>) -> T {
    match result {
        Ok(r) => r,
        Err(es) => {
            es.iter().for_each(|e| eprintln!("{}", e));
            exit(1);
        }
    }
}

fn create_vm<'a>(
    literals: Vec<Literal<'a>>,
    source_code_locations: Vec<SourceCodeLocation<'a>>,
    rom: Vec<Instruction>,
) -> Result<Vec<u8>, Error> {
    let mut last_address = 0usize;
    let mut constants = vec![];
    let mut locations = vec![];
    let mut location_indexes = HashMap::new();
    let mut memory = vec![];
    let mut string_address = HashMap::new();
    for c in literals.iter() {
        match c {
            Literal::QuotedString(s) => {
                if string_address.get(s).is_none() {
                    let address = last_address;
                    last_address += s.len();
                    string_address.insert(*s, address);
                    memory.extend_from_slice(s.as_bytes());
                    constants.push(Value::String(address));
                } else {
                    constants.push(Value::String(string_address[*s]));
                }
            }
            Literal::Integer(i) => constants.push(Value::Integer(*i)),
            Literal::Float(f) => constants.push(Value::Float(*f)),
            Literal::Keyword(DataKeyword::True) => constants.push(Value::Bool(true)),
            Literal::Keyword(DataKeyword::False) => constants.push(Value::Bool(false)),
            Literal::Keyword(DataKeyword::Nil) => constants.push(Value::Nil),
        }
    }

    for scl in source_code_locations.iter() {
        if location_indexes.get(scl).is_none() {
            location_indexes.insert(scl.clone(), location_indexes.len());
            let address = if string_address.get(&scl.file).is_none() {
                let address = last_address;
                last_address += scl.file.len();
                memory.extend_from_slice(scl.file.as_bytes());
                constants.push(Value::String(address));
                string_address.insert(scl.file, address);
                address
            } else {
                string_address[scl.file]
            };
            locations.push(Location {
                line: scl.line,
                address,
            });
        }
    }
    Ok(to_bytes(&constants, &locations, &memory, &rom))
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
    let ss = handle_result(result);
    let mut resolver = Resolver::new();
    let locals = handle_result(resolver.run(&ss));
    let mut c = compiler::Compiler::new(locals);
    let instructions = handle_result(c.run(&ss));
    let (literals, locations) = (c.constants, c.locations);
    let vm = create_vm(literals, locations, instructions).unwrap();
    output.write_all(&vm).unwrap();
}
