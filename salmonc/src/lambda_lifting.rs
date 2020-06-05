use parser::types::{Statement, ProgramError, MutPass};
use ahash::{AHashMap as HashMap};

fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

pub struct LambdaLifting<'a> {
    function_counter: usize,
    locals: HashMap<usize, usize>,
    output: Vec<Statement<'a>>,
    scopes: Vec<HashMap<&'a str, &'a str>>,
}

impl<'a> LambdaLifting<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> LambdaLifting<'a> {
        LambdaLifting {
            function_counter: 0,
            output: Vec::default(),
            scopes: vec![HashMap::default()],
            locals,
        }
    }
}

impl<'a> MutPass<'a, Vec<Statement<'a>>> for LambdaLifting<'a> {
    fn run(&mut self, ss: &'a mut [Statement<'a>]) -> Result<Vec<Statement<'a>>, Vec<ProgramError<'a>>> {
        for s in ss {
            if self.scopes.len() == 1 {
                self.output.push(s.clone());
            }
            self.pass(s)?;
        }
        Ok(self.output.clone())
    }

    fn pass_function_declaration(
        &mut self,
        name: &'a mut &'a str,
        _arguments: &'a mut [&'a str],
        body: &'a mut Vec<Box<Statement<'a>>>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.scopes.push(HashMap::default());
        for s in body.iter_mut() {
            self.pass(s)?;
        }
        self.scopes.pop();
        let new_function_name = string_to_static_str(format!("@function{}", self.function_counter));
        *name = new_function_name;
        let scope = self.scopes.len()-1;
        self.scopes[scope].insert(*name, new_function_name);
        Ok(())
    }

    fn pass_variable_literal(
        &mut self,
        identifier: & mut &'a str,
        expression_id: usize,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(scope) = self.locals.get(&expression_id).cloned() {
            if let Some(new_identifier) = self.scopes[scope].get(*identifier).cloned() {
                *identifier = new_identifier;
            }
        }
        Ok(())
    }

    fn pass_anonymous_function(
        &mut self,
        _arguments: &'a mut [&'a str],
        body: &'a mut [Statement<'a>],
        _expression: usize,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.scopes.push(HashMap::default());
        for s in body {
            self.pass(s)?;
        }
        self.scopes.pop();
        Ok(())
    }
}