use crate::types::{Statement, StatementType, Pass, ProgramError, SourceCodeLocation, Expression};
use std::path::Path;
use std::fs::File;
use std::io::Read;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;
use crate::resolver::{WithScopedVariables, Resolver};
use std::collections::HashMap;

struct SimpleWithScopedVariables {
    locals: HashMap<usize, usize>,
}

impl WithScopedVariables for SimpleWithScopedVariables {
    fn resolve_variable(&mut self, expression: &Expression, scope_id: usize) {
        self.locals.insert(expression.id(), scope_id);
    }
}

pub trait WithImports {
    fn add_import(&mut self, name: &str, statements: Vec<Statement>, locals: &HashMap<usize, usize>) -> Result<(), Vec<ProgramError>>;
}

pub struct Importer<'a, T: WithImports> {
    paths: &'a [String],
    with_imports: Rc<RefCell<T>>,
}

impl<'a, T: WithImports> Importer<'a, T> {
    pub fn new(paths: &'a [String], with_imports: Rc<RefCell<T>>) -> Importer<'a, T> {
        Importer { paths, with_imports }
    }
    pub fn open_import(&self, name: &str, location: &SourceCodeLocation) -> Result<String, ProgramError> {
        for path in self.paths {
            let path = Path::new(path).join(format!("{}.sa", name));
            if path.exists() {
                let mut buffer = String::new();
                File::open(path).unwrap().read_to_string(&mut buffer).unwrap();
                return Ok(buffer);
            }
        }
        Err(ProgramError {
            location: location.clone(),
            message: format!("Can't find file {}.sa", name),
        })
    }
    pub fn resolve_import(&self, name: &str, location: &SourceCodeLocation) -> Result<Vec<Statement>, Vec<ProgramError>> {
        let content = self.open_import(name, location)
            .map_err(|e| vec![e])?;
        let mut lexer = Lexer::new(content, name.to_owned());
        lexer.parse()
            .and_then(|tt| {
                let parser = Parser::new(tt.into_iter().peekable());
                parser.parse()
            })
    }
}

impl<'a, 'b, T: WithImports + WithScopedVariables> Pass<'a> for Importer<'b, T> {
    fn run(&mut self, ss: &'a [Statement]) -> Result<(), Vec<ProgramError>> {
        let mut errors = vec![];
        for s in ss {
            if let StatementType::Import { name } = &s.statement_type {
                match self.resolve_import(name, &s.location) {
                    Ok(ss) => {
                        let variables = Rc::new(RefCell::new(SimpleWithScopedVariables {
                            locals: HashMap::default(),
                        }));
                        let mut resolver = Resolver::new(variables.clone());
                        let result = resolver.run(&ss)
                            .and_then(|_| {
                                self.with_imports.borrow_mut().add_import(name, ss, &variables.borrow().locals)
                            });
                        if let Err(new_errors) = result {
                            errors.extend(new_errors);
                        }
                    }
                    Err(new_errors) => {
                        errors.extend(new_errors);
                    }
                }
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}