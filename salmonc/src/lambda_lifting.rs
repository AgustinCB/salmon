use parser::types::{Statement, ProgramError, Pass, StatementType, SourceCodeLocation, ExpressionType, Expression};
use ahash::{AHashMap as HashMap};

fn leak_reference<'a, T>(s: T) -> &'a T {
    Box::leak(Box::new(s))
}

pub struct LambdaLifting<'a> {
    change_name: bool,
    changes: HashMap<usize, ExpressionType<'a>>,
    current_location: Option<SourceCodeLocation<'a>>,
    function_counter: usize,
    locals: HashMap<usize, usize>,
    missed_locals: Vec<Vec<&'a str>>,
    output: Vec<Statement<'a>>,
    scopes: Vec<HashMap<&'a str, &'a str>>,
}

impl<'a> LambdaLifting<'a> {
    pub fn new(locals: HashMap<usize, usize>) -> LambdaLifting<'a> {
        LambdaLifting {
            change_name: true,
            changes: HashMap::default(),
            current_location: None,
            function_counter: 0,
            missed_locals: Vec::default(),
            output: Vec::default(),
            scopes: vec![HashMap::default()],
            locals,
        }
    }
}

type LambdaLiftingResult<'a> = (Vec<Statement<'a>>, HashMap<usize, ExpressionType<'a>>);

impl<'a> Pass<'a, LambdaLiftingResult<'a>> for LambdaLifting<'a> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<LambdaLiftingResult<'a>, Vec<ProgramError<'a>>> {
        for s in ss {
            self.current_location = Some(s.location.clone());
            if self.scopes.len() == 1 && !s.is_function_declaration() {
                self.output.push(s.clone());
            }
            self.pass(s)?;
        }
        Ok((self.output.clone(), self.changes.clone()))
    }

    fn pass_function_declaration(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: &'a [Box<Statement<'a>>],
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let new_function_name = if self.change_name {
            let new_function_name = leak_reference(format!("@function{}", self.function_counter));
            self.function_counter += 1;
            let scope = self.scopes.len()-1;
            self.scopes[scope].insert(name, new_function_name);
            new_function_name
        } else {
            name
        };
        self.change_name = true;
        self.scopes.push(HashMap::default());
        self.missed_locals.push(Vec::default());
        for s in body.iter() {
            self.pass(s)?;
        }
        self.scopes.pop();
        let missed_locals = self.missed_locals.pop().unwrap();
        self.output.push(Statement {
            location: statement.location.clone(),
            statement_type: StatementType::FunctionDeclaration {
                name: new_function_name,
                arguments: arguments.to_vec(),
                body: body.to_vec(),
                context_variables: missed_locals,
            },
        });
        Ok(())
    }

    fn pass_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let expression_id = expression.id();
        if let Some(scope) = self.locals.get(&expression_id).cloned() {
            if let Some(new_identifier) = self.scopes[scope].get(identifier).cloned() {
                self.changes.insert(expression_id, ExpressionType::VariableLiteral {
                    identifier: new_identifier,
                });
            } else if scope > 0 && scope < self.missed_locals.len() - 1 {
                self.missed_locals.last_mut().unwrap().push(identifier);
            }
        }
        Ok(())
    }

    fn pass_variable_assignment(
        &mut self,
        _identifier: &'a str,
        value: &'a Expression<'a>,
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(value)?;
        Ok(())
    }

    fn pass_anonymous_function(
        &mut self,
        arguments: &'a [&'a str],
        body: &'a [Statement<'a>],
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let new_function_name = leak_reference(format!("@function{}", self.function_counter));
        self.function_counter += 1;
        let function_definition = Statement {
            statement_type: StatementType::FunctionDeclaration {
                name: new_function_name,
                arguments: arguments.to_vec(),
                body: body.iter().cloned().map(Box::new).collect(),
                context_variables: vec![],
            },
            location: self.current_location.clone().unwrap(),
        };
        /*
         * This is a bit of black magic. What is happening here:
         * - We get the static, raw pointer to a leaked reference to function_definition.
         * - We "pass" that pointer.
         * - We then wrap it again into a Box to make sure it's properly cleaned after.
         * Why we can't use references:
         * - We cannot use a reference to `function-definition`, because is a scope lived and would
         * not live for 'a.
         * - We cannot use a reference to the last element in self.output because self does not live
         * for 'a.
         * - We cannot leak a Box, because we won't have any way to clean after: Since `pass` asks
         * for the statement to live for 'a, and Box::from_raw requires a mutable reference, we won't
         * be able to make the conversion.
         * Why is this safe:
         * The danger is that pass would store a reference to something inside function_definition
         * that would later be dropped. However, this class does not do that and rather copies all
         * values.
         */
        let r = Box::into_raw(Box::new(function_definition));
        self.change_name = false;
        self.pass(unsafe { r.as_ref() }.unwrap())?;
        let _ = unsafe { Box::from_raw(r) };
        self.changes.insert(expression.id(), ExpressionType::VariableLiteral {
            identifier: new_function_name,
        });
        Ok(())
    }
}