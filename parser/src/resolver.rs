use crate::types::{Expression, ExpressionType, ProgramError, SourceCodeLocation, Statement, StatementType, Pass};
use ahash::{AHashMap as HashMap};

pub struct Resolver<'a> {
    check_used: bool,
    scopes: Vec<HashMap<&'a str, bool>>,
    uses: Vec<HashMap<&'a str, usize>>,
    locations: Vec<HashMap<&'a str, &'a SourceCodeLocation<'a>>>,
    locals: HashMap<usize, usize>,
}

impl<'a> Resolver<'a> {
    pub fn new() -> Resolver<'a> {
        Resolver {
            check_used: true,
            locals: HashMap::default(),
            locations: vec![HashMap::default()],
            scopes: vec![HashMap::default()],
            uses: vec![HashMap::default()],
        }
    }
    pub fn new_without_check_used() -> Resolver<'a> {
        Resolver {
            check_used: false,
            locals: HashMap::default(),
            locations: vec![HashMap::default()],
            scopes: vec![HashMap::default()],
            uses: vec![HashMap::default()],
        }
    }
    fn push_scope(&mut self, scope: HashMap<&'a str, bool>) {
        self.scopes.push(scope);
        self.uses.push(HashMap::default());
        self.locations.push(HashMap::default());
    }
    fn dry_pop_scope(&mut self) {
        self.scopes.pop();
        self.uses.pop();
        self.locations.pop();
    }
    fn pop_scope(&mut self) -> Result<(), Vec<ProgramError<'a>>> {
        self.scopes.pop();
        if let (Some(mut variables), Some(locations)) = (self.uses.pop(), self.locations.pop()) {
            variables.insert("this", 1);
            let errors: Vec<ProgramError<'a>> = variables
                .iter()
                .filter(|(_, uses)| self.check_used && **uses == 0)
                .map(|p| ProgramError {
                    message: format!("Variable `{}` never used.", *p.0),
                    location: locations[*p.0].clone(),
                })
                .collect();
            if !errors.is_empty() {
                return Err(errors);
            }
        }
        Ok(())
    }
    fn declare(
        &mut self,
        name: &'a str,
        location: &'a SourceCodeLocation<'a>,
    ) -> Result<(), ProgramError<'a>> {
        if let Some(s) = self.scopes.last_mut() {
            if s.contains_key(name) {
                return Err(ProgramError {
                    message: format!("Variable `{}` already declared in this scope!", name),
                    location: location.clone(),
                });
            }
            s.insert(name, false);
            if let Some(locations) = self.locations.last_mut() {
                locations.insert(name, location);
            }
            if let Some(uses) = self.uses.last_mut() {
                uses.insert(name, 0);
            }
        }
        Ok(())
    }
    fn define(&mut self, name: &'a str) {
        if let Some(s) = self.scopes
            .iter_mut()
            .rev()
            .find(|s| s.contains_key(name)) {
            s.insert(name, true);
        }
    }
    fn resolve_local(&mut self, expression: &Expression<'a>, name: &'a str) -> Result<(), ProgramError<'a>> {
        if let Some((i, scope)) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find(|(_, s)| s.contains_key(name))
        {
            if !scope[name] {
                return Err(ProgramError {
                    message: format!("`{}` used without being initialized.", name),
                    location: expression.location.clone(),
                })
            }
            let new_uses = self.uses[i][name] + 1;
            self.uses[i].insert(name, new_uses);
            self.locals.insert(expression.id(), i);
        }
        Ok(())
    }
    fn resolve_functions(
        &mut self,
        methods: &'a [Box<Statement<'a>>],
        check_defined: bool,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        methods
            .iter()
            .map(|s| {
                if let StatementType::FunctionDeclaration { name, .. } = &s.statement_type {
                    if !check_defined {
                        self.scopes.last_mut().map(|h| h.remove(*name));
                    }
                }
                let r = self.pass(s);
                if let StatementType::FunctionDeclaration { name, .. } = &s.statement_type {
                    self.uses.last_mut().unwrap().insert(name, 1);
                }
                r
            })
            .collect::<Result<_, Vec<ProgramError<'a>>>>()?;
        Ok(())
    }
    fn resolve_function<'b>(
        &mut self,
        arguments: &'a [&'a str],
        context_variables: Option<&'a [&'a str]>,
        body: &'b [&'a Statement<'a>],
        location: &'a SourceCodeLocation<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.push_scope(HashMap::default());
        for arg in arguments {
            self.declare(arg, location).map_err(|e| vec![e])?;
            self.define(arg);
        }
        if let Some(context_variables) = context_variables {
            for arg in context_variables {
                self.declare(arg, location).map_err(|e| vec![e])?;
                self.define(arg);
            }
        }
        body.iter()
            .map(|s| self.pass(s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        self.pop_scope()?;
        Ok(())
    }
}

impl<'a> Pass<'a, HashMap<usize, usize>> for Resolver<'a> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<HashMap<usize, usize>, Vec<ProgramError<'a>>> {
        ss.iter()
            .map(|s| self.pass(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError<'a>>>>()?;
        Ok(self.locals.clone())
    }

    fn pass_module(
        &mut self,
        name: &'a str,
        statements: &'a [Box<Statement<'a>>],
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        self.define(name);
        self.push_scope(HashMap::default());
        statements.iter()
            .map(|s| self.pass(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        self.dry_pop_scope();
        Ok(())
    }

    fn pass_import(
        &mut self,
        name: &'a str,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        self.define(name);
        Ok(())
    }

    fn pass_block(&mut self, body: &'a [Box<Statement<'a>>]) -> Result<(), Vec<ProgramError<'a>>> {
        self.push_scope(HashMap::default());
        body.iter()
            .map(|s| self.pass(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        self.pop_scope()
    }

    fn pass_variable_declaration(
        &mut self,
        name: &'a str,
        expression: &'a Option<Expression<'a>>,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        if let Some(e) = expression {
            self.pass_expression(e)?;
            self.define(name);
        }
        Ok(())
    }

    fn pass_class_declaration(
        &mut self,
        name: &'a str,
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        superclass: &'a Option<Expression<'a>>,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        if let Some(e) = superclass {
            if let ExpressionType::VariableLiteral { identifier } = &e.expression_type {
                if *identifier == name {
                    return Err(vec![ProgramError {
                        message: "A class cannot inherit from itself.".to_owned(),
                        location: statement.location.clone(),
                    }]);
                }
                self.resolve_local(e, identifier)
                    .map_err(|e| vec![e])?;
            } else {
                self.pass_expression(e)?;
            }
        }
        self.push_scope(HashMap::default());
        self.resolve_functions(methods, true)?;
        self.resolve_functions(getters, false)?;
        self.resolve_functions(setters, false)?;
        self.resolve_functions(static_methods, true)?;
        self.pop_scope()?;
        self.define(&name);
        Ok(())
    }

    fn pass_trait_declaration(
        &mut self,
        name: &'a str,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        self.define(name);
        Ok(())
    }

    fn pass_trait_implementation(
        &mut self,
        class_name: &'a Expression<'a>,
        trait_name: &'a Expression<'a>,
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(class_name)?;
        self.pass_expression(trait_name)?;
        self.push_scope(HashMap::default());
        self.resolve_functions(methods, true)?;
        self.resolve_functions(getters, false)?;
        self.resolve_functions(setters, false)?;
        self.resolve_functions(static_methods, true)?;
        self.pop_scope()
    }

    fn pass_function_declaration(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: &'a [Box<Statement<'a>>],
        statement: &'a Statement<'a>,
        context_variables: &'a [&'a str],
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.declare(name, &statement.location)
            .map_err(|e| vec![e])?;
        self.define(name);
        let body = body.iter().map(|s| &(**s)).collect::<Vec<&Statement>>();
        self.resolve_function(
            arguments, Some(context_variables), &body, &statement.location
        )
    }

    fn pass_module_literal(
        &mut self,
        module: &'a str,
        field: &'a Expression<'a>,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(field)?;
        self.resolve_local(expression, module).map_err(|e| vec![e])
    }

    fn pass_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.resolve_local(expression, identifier).map_err(|e| vec![e])
    }

    fn pass_variable_assignment(
        &mut self,
        identifier: &'a str,
        expression_value: &'a Expression<'a>,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.define(identifier);
        self.pass_expression(expression_value)?;
        self.resolve_local(expression, identifier).map_err(|e| vec![e])
    }

    fn pass_anonymous_function(
        &mut self,
        arguments: &'a [&'a str],
        body: &'a [Statement<'a>],
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let body = body.iter().collect::<Vec<&'a Statement>>();
        self.resolve_function(arguments, None, &body, &expression.location)
    }
}
