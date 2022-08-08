#![feature(exact_size_is_empty)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(pattern)]

use std::collections::HashMap;
use parser::lexer::Lexer;
use parser::parser::Parser;
use parser::resolver::Resolver;
use parser::types::{MutPass, Pass, ProgramError, Literal, DataKeyword, SourceCodeLocation, Statement, DebugStatements};
use std::env;
use std::env::Args;
use std::io::{Read, Write};
use std::fs::File;
use std::process::exit;
use smoked::cpu::{Value, Location, USIZE_SIZE};
use smoked::instruction::Instruction;
use smoked::serde::to_bytes;
use crate::compiler::{ConstantValues, ClassMembers};
use crate::lambda_lifting::LambdaLifting;
use std::mem::size_of;

mod changes;
mod compiler;
mod lambda_lifting;

struct Config {
    paths: Vec<String>,
    input: Option<String>,
    output: Option<String>,
    show_instructions: bool,
    show_statements: bool,
    show_resulting_statements: bool,
}

fn parse_config(args: &mut Args) -> Config {
    let mut input = None;
    let mut output = None;
    let mut paths = vec![".".to_owned()];
    let mut show_instructions = false;
    let mut show_statements = false;
    let mut show_resulting_statements = false;
    while !args.is_empty() {
        let arg = args.next().unwrap();
        match arg.as_str() {
            "-p" | "--path" => {
                paths.push(args.next().expect("Expected path"))
            },
            "-i" | "--instructions" => {
                show_instructions = true;
            },
            "-s" | "--show-statements" => {
                show_statements = true;
            },
            "-r" | "--show-resulting-statements" => {
                show_resulting_statements = true;
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
        show_instructions,
        show_resulting_statements,
        show_statements,
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
    literals: Vec<ConstantValues<'a>>,
    class_members: HashMap<&'a str, ClassMembers<'a>>,
    source_code_locations: Vec<SourceCodeLocation<'a>>,
    rom: Vec<Instruction>,
) -> Vec<u8> {
    let mut last_address = 0usize;
    let mut constants = vec![];
    let mut locations = vec![];
    let mut location_indexes: HashMap<SourceCodeLocation<'a>, usize> = HashMap::default();
    let mut memory = vec![];
    let mut string_address: HashMap<&'a str, usize> = HashMap::default();
    let mut functions: HashMap<&'a str, Value> = HashMap::default();
    for (i, c) in literals.iter().enumerate() {
        match c {
            ConstantValues::Literal(Literal::QuotedString(s)) => {
                if string_address.get(s).is_none() {
                    let address = last_address;
                    last_address += s.len();
                    string_address.insert(s, address);
                    memory.extend_from_slice(s.as_bytes());
                    constants.push((i, Value::String(address)));
                } else {
                    constants.push((i, Value::String(string_address[*s])));
                }
            }
            ConstantValues::Literal(Literal::Integer(v)) => constants.push((i, Value::Integer(*v))),
            ConstantValues::Literal(Literal::Float(f)) => constants.push((i, Value::Float(*f))),
            ConstantValues::Literal(Literal::Keyword(DataKeyword::True)) => constants.push((i, Value::Bool(true))),
            ConstantValues::Literal(Literal::Keyword(DataKeyword::False)) => constants.push((i, Value::Bool(false))),
            ConstantValues::Literal(Literal::Keyword(DataKeyword::Nil)) => constants.push((i, Value::Nil)),
            ConstantValues::Function { arity, ip, name, .. } => {
                let function = Value::Function { ip: *ip, arity: *arity, uplifts: None };
                functions.insert(name, function.clone());
                constants.push((i, function))
            },
            _ => {}
        }
    }

    for (i, c) in literals.iter().enumerate() {
        match c {
            ConstantValues::Class { name } => {
                let members_length = class_members.get(name).unwrap().length();
                let members_length_bytes: &[u8] = unsafe {
                    std::slice::from_raw_parts(&members_length as *const usize as *const u8, size_of::<usize>())
                };
                let mut members_bytes = members_length_bytes.to_vec();
                class_members.get(name).unwrap().for_each_member_in_order(|name, function| {
                    let m = functions.get(function).unwrap().clone();
                    let content: &[u8] = unsafe {
                        std::slice::from_raw_parts(&m as *const Value as *const u8, size_of::<Value>())
                    };
                    let k = string_address.get(name).unwrap().clone();
                    let p: &[u8] = unsafe {
                        std::slice::from_raw_parts(&k as *const usize as *const u8, size_of::<usize>())
                    };
                    members_bytes.extend_from_slice(p);
                    members_bytes.extend_from_slice(content);
                });
                let capacity = members_bytes.len();
                let address = last_address;
                last_address += USIZE_SIZE;
                let last_address_bytes: &[u8] = unsafe {
                    std::slice::from_raw_parts(&last_address as *const usize as *const u8, size_of::<usize>())
                };
                memory.extend_from_slice(last_address_bytes);
                last_address += capacity;
                memory.extend_from_slice(&members_bytes);
                let tags = last_address;
                let name_bytes = name.as_bytes();
                memory.extend_from_slice(name_bytes);
                last_address += name_bytes.len();
                constants.push((i, Value::Object { address, tags }));
            }
            _ => {}
        }
    }
    constants.sort_by(|v1, v2| v1.0.cmp(&v2.0));
    let mut constants = constants.into_iter().map(|v| v.1).collect::<Vec<_>>();

    for scl in source_code_locations.iter() {
        if location_indexes.get(scl).is_none() {
            let new_value = location_indexes.len();
            location_indexes.insert(scl.clone(), new_value);
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
    to_bytes(&constants, &locations, &memory, &rom)
}

fn rearrenge_function_declarations(ss: Vec<Statement>) -> Vec<Statement> {
    let mut functions = vec![];
    let mut traits = vec![];
    let mut others = vec![];
    for s in ss {
        if s.is_function_declaration() || s.is_class_declaration() {
            functions.push(s);
        } else if s.is_trait_declaration() {
            traits.push(s);
        } else {
            others.push(s);
        }
    }
    functions.extend_from_slice(&traits);
    functions.extend_from_slice(&others);
    functions
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
    let (ss, statement_factory, expression_factory) = handle_result(result);
    if config.show_statements {
        eprintln!("{:?}", DebugStatements(&ss));
    }
    let locals = handle_result(Resolver::new().run(&ss));
    let (mut ss, changes, statement_changes) = handle_result(LambdaLifting::new(locals.clone(), statement_factory, expression_factory).run(&ss));
    let ss_ref: *mut Vec<Statement<'_>> = &mut ss as *mut _;
    handle_result(changes::Changes::new(changes, statement_changes).run(unsafe { ss_ref.as_mut() }.unwrap()));
    let ss = rearrenge_function_declarations(ss);
    if config.show_resulting_statements {
        eprintln!("{:?}", DebugStatements(&ss));
    }
    let locals = handle_result(Resolver::new_without_check_used().run(&ss));
    let mut c = compiler::Compiler::new(locals);
    let instructions = handle_result(c.run(&ss));
    let (literals, locations, class_members) = (c.constants, c.locations, c.class_members);
    if config.show_instructions {
        eprintln!("Instructions: {:?}", instructions);
        eprintln!("Literals: {:?}", &literals);
    } else {
        let vm = create_vm(literals, class_members, locations, instructions);
        output.write_all(&vm).unwrap();
    }
}
