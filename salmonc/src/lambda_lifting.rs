use parser::types::{Statement, ProgramError, Pass, StatementType, SourceCodeLocation, ExpressionType, Expression, StatementFactory, ExpressionFactory, TokenType};
use ahash::{AHashMap as HashMap, AHashSet as HashSet};

pub fn leak_reference<'a, T>(s: T) -> &'a T {
    Box::leak(Box::new(s))
}

pub struct LambdaLifting<'a> {
    change_name: bool,
    changes: HashMap<usize, ExpressionType<'a>>,
    class_counter: usize,
    class_scope: Option<usize>,
    current_location: Option<SourceCodeLocation<'a>>,
    expression_factory: ExpressionFactory,
    function_counter: usize,
    locals: HashMap<usize, usize>,
    missed_locals: Vec<HashSet<&'a str>>,
    output: Vec<Statement<'a>>,
    scopes: Vec<HashMap<&'a str, &'a str>>,
    statement_factory: StatementFactory,
    statement_changes: HashMap<usize, StatementType<'a>>,
    trait_counter: usize,
}

impl<'a> LambdaLifting<'a> {
    pub fn new(
        locals: HashMap<usize, usize>,
        statement_factory: StatementFactory,
        expression_factory: ExpressionFactory,
    ) -> LambdaLifting<'a> {
        LambdaLifting {
            change_name: true,
            changes: HashMap::default(),
            class_counter: 0,
            class_scope: None,
            current_location: None,
            function_counter: 0,
            missed_locals: Vec::default(),
            output: Vec::default(),
            scopes: vec![HashMap::default()],
            statement_changes: HashMap::default(),
            trait_counter: 0,
            expression_factory,
            locals,
            statement_factory,
        }
    }

    fn create_new_function(
        &mut self,
        arguments: Option<&'a [&'a str]>,
        body: Vec<Box<Statement<'a>>>,
    ) -> Result<&'static str, Vec<ProgramError<'a>>> {
        let new_function_name = leak_reference(format!("@function{}", self.function_counter));
        self.function_counter += 1;
        let function_definition = self.statement_factory.new_statement(
            self.current_location.clone().unwrap(),
            StatementType::FunctionDeclaration {
                name: new_function_name,
                arguments: arguments.map(|a| a.to_vec()).unwrap_or(vec![]),
                context_variables: vec![],
                body,
            },
        );
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
        Ok(new_function_name)
    }

    fn class_methods_to_variable_accessors(&mut self, methods: &'a [Box<Statement<'a>>]) -> Result<Vec<Box<Statement<'a>>>, Vec<ProgramError<'a>>> {
        methods.iter().map(|s| {
            self.pass(s)?;
            if let (StatementType::FunctionDeclaration { name, .. },
                StatementType::FunctionDeclaration { name: new_name, .. }) =
                (&s.statement_type, &self.output.last().unwrap().statement_type) {
                let right = Box::new(self.expression_factory.new_expression(
                    ExpressionType::VariableLiteral { identifier: name },
                    s.location.clone(),
                ));
                let left = Box::new(self.expression_factory.new_expression(
                    ExpressionType::VariableLiteral { identifier: new_name },
                    s.location.clone(),
                ));
                let expression = self.expression_factory.new_expression(
                    ExpressionType::Binary { left, right, operator: TokenType::Comma, },
                    s.location.clone(),
                );
                Ok(Box::new(self.statement_factory.new_statement(
                    s.location.clone(),
                    StatementType::Expression { expression },
                )))
            } else {
                panic!("Last statement should be a function!")
            }
        }).collect()
    }

    fn change_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Option<ExpressionType<'a>> {
        let expression_id = expression.id();
        if let Some(scope) = self.locals.get(&expression_id).cloned() {
            if scope > 0 && scope < self.scopes.len() - 1 {
                if self.class_scope.unwrap_or(0) == scope {
                    return Some(ExpressionType::Get {
                        callee: Box::new(self.expression_factory.new_expression(
                            ExpressionType::VariableLiteral {
                                identifier: "this",
                            },
                            expression.location.clone()
                        )),
                        property: identifier,
                    });
                } else {
                    self.missed_locals.last_mut().unwrap().insert(identifier);
                }
            }
            if let Some(new_identifier) = self.scopes.get(scope).map(|v| v.get(identifier)).flatten().cloned() {
                Some(ExpressionType::VariableLiteral {
                    identifier: new_identifier,
                })
            } else {
                None
            }
        } else {
            None
        }
    }
}

type LambdaLiftingResult<'a> = (Vec<Statement<'a>>, HashMap<usize, ExpressionType<'a>>, HashMap<usize, StatementType<'a>>);

pub fn variable_or_module_name<'a>(expression: &'a Expression<'a>) -> Result<&'a str, Vec<ProgramError<'a>>> {
    match expression.expression_type {
        ExpressionType::VariableLiteral { identifier } => Ok(identifier),
        _ => Err(vec![ProgramError {
            location: expression.location.clone(),
            message: "Expected variable literal".to_string()
        }])?,
    }
}

impl<'a> Pass<'a, LambdaLiftingResult<'a>> for LambdaLifting<'a> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<LambdaLiftingResult<'a>, Vec<ProgramError<'a>>> {
        for s in ss {
            self.current_location = Some(s.location.clone());
            if self.scopes.len() == 1 && !s.is_function_declaration() &&
                !s.is_class_declaration() && !s.is_trait_declaration() {
                self.output.push(s.clone());
            }
            self.pass(s)?;
        }
        Ok((self.output.clone(), self.changes.clone(), self.statement_changes.clone()))
    }

    fn pass_block(
        &mut self,
        body: &'a [Box<Statement<'a>>],
        statement_id: usize,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let mut body = body.to_vec();
        body.push(Box::new(self.statement_factory.new_statement(
            self.current_location.clone().unwrap(),
            StatementType::Return { value: None },
        )));
        let new_function_name = self.create_new_function(None, body)?;
        let callee = Box::new(self.expression_factory.new_expression(
            ExpressionType::VariableLiteral {
                identifier: new_function_name,
            },
            self.current_location.clone().unwrap(),
        ));
        let expression = ExpressionType::Binary {
            left: Box::new(self.expression_factory.new_expression(
                ExpressionType::UpliftFunctionVariables(new_function_name),
                self.current_location.clone().unwrap(),
            )),
            operator: TokenType::Bar,
            right: Box::new(self.expression_factory.new_expression(
                ExpressionType::Call {
                    arguments: vec![],
                    callee,
                },
                self.current_location.clone().unwrap(),
            )),
        };
        self.statement_changes.insert(statement_id, StatementType::Expression {
            expression: self.expression_factory.new_expression(
                expression,
                self.current_location.clone().unwrap()
            ),
        });
        Ok(())
    }

    fn pass_class_declaration(
        &mut self,
        name: &'a str,
        properties: &'a [Box<Statement<'a>>],
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        superclass: &'a Option<Expression<'a>>,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let new_class_name = leak_reference(format!("@class{}", self.class_counter));
        self.class_counter += 1;
        let scope = self.scopes.len()-1;
        self.scopes[scope].insert(name, new_class_name);
        self.scopes.push(HashMap::default());
        self.class_scope = Some(scope+1);
        let new_methods = self.class_methods_to_variable_accessors(methods)?;
        let new_static_methods = self.class_methods_to_variable_accessors(static_methods)?;
        let new_getters = self.class_methods_to_variable_accessors(getters)?;
        let new_setters = self.class_methods_to_variable_accessors(setters)?;
        let new_superclass = match superclass {
            Some(s) => {
                match &s.expression_type {
                    ExpressionType::VariableLiteral { identifier } => {
                        self.change_variable_literal(identifier, s)
                            .map(|et| {
                                self.expression_factory.new_expression(et, s.location.clone())
                            })
                    }
                    _ => None,
                }
            },
            _ => None,
        };
        self.scopes.pop();
        self.class_scope = None;
        self.output.push(self.statement_factory.new_statement(
            statement.location.clone(),
            StatementType::ClassDeclaration {
                name: new_class_name,
                properties: properties.to_vec(),
                superclass: new_superclass,
                methods: new_methods,
                static_methods: new_static_methods,
                getters: new_getters,
                setters: new_setters,
            },
        ));
        Ok(())
    }

    fn pass_trait_declaration(
        &mut self,
        name: &'a str,
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let new_trait_name = leak_reference(format!("@trait{}", self.trait_counter));
        self.trait_counter += 1;
        let scope = self.scopes.len()-1;
        self.scopes[scope].insert(name, new_trait_name);
        if let StatementType::TraitDeclaration {
            methods, getters, setters, static_methods, ..
        } = &statement.statement_type {
            self.output.push(self.statement_factory.new_statement(
                statement.location.clone(),
                StatementType::TraitDeclaration {
                    name: new_trait_name,
                    methods: methods.clone(),
                    getters: getters.clone(),
                    setters: setters.clone(),
                    static_methods: static_methods.clone()
                }
            ));
        }
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
        statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let (Some(trait_scope), Some(class_scope)) =
            (self.locals.get(&trait_name.id()).cloned(), self.locals.get(&class_name.id()).cloned()) {
            let class_name_value = variable_or_module_name(class_name)?;
            let trait_name_value = variable_or_module_name(trait_name)?;
            if let (Some(new_class_name), Some(new_trait_name)) =
                (self.scopes[class_scope].get(class_name_value).cloned(), self.scopes[trait_scope].get(trait_name_value).cloned()) {
                self.scopes.push(HashMap::default());
                let new_methods = self.class_methods_to_variable_accessors(methods)?;
                let new_static_methods = self.class_methods_to_variable_accessors(static_methods)?;
                let new_getters = self.class_methods_to_variable_accessors(getters)?;
                let new_setters = self.class_methods_to_variable_accessors(setters)?;
                self.scopes.pop();
                self.statement_changes.insert(statement.id(), StatementType::TraitImplementation {
                    class_name: self.expression_factory.new_expression(
                        ExpressionType::VariableLiteral { identifier: new_class_name },
                        class_name.location.clone(),
                    ),
                    trait_name: self.expression_factory.new_expression(
                        ExpressionType::VariableLiteral { identifier: new_trait_name },
                        trait_name.location.clone(),
                    ),
                    methods: new_methods,
                    static_methods: new_static_methods,
                    getters: new_getters,
                    setters: new_setters,
                });
            }
            if trait_scope > 0 && trait_scope < self.scopes.len() - 1 {
                self.missed_locals.last_mut().unwrap().insert(trait_name_value);
            }
            if class_scope > 0 && class_scope < self.scopes.len() - 1 {
                self.missed_locals.last_mut().unwrap().insert(class_name_value);
            }
        }
        Ok(())
    }

    fn pass_function_declaration(
        &mut self,
        name: &'a str,
        arguments: &'a [&'a str],
        body: &'a [Box<Statement<'a>>],
        statement: &'a Statement<'a>,
        _context_variables: &'a [&'a str],
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
        self.missed_locals.push(HashSet::default());
        let mut new_body = vec![];
        for s in body.iter() {
            self.pass(s)?;
            let statement = if s.is_function_declaration() {
                if let StatementType::FunctionDeclaration { name, .. } = s.statement_type {
                    Box::new(self.statement_factory.new_statement(
                        s.location.clone(),
                        StatementType::Expression {
                            expression: self.expression_factory.new_expression(
                                ExpressionType::UpliftFunctionVariables(self.scopes[self.scopes.len()-1][name]),
                                s.location.clone(),
                            )
                        },
                    ))
                } else {
                    panic!("Last statement should be a function!")
                }
            } else {
                s.clone()
            };
            new_body.push(statement);
        }
        self.scopes.pop();
        let missed_locals = self.missed_locals.pop().unwrap().iter().cloned().collect::<Vec<&str>>();
        self.output.push(self.statement_factory.new_statement(
            statement.location.clone(),
            StatementType::FunctionDeclaration {
                name: new_function_name,
                arguments: arguments.to_vec(),
                body: new_body,
                context_variables: missed_locals,
            },
        ));
        Ok(())
    }

    fn pass_variable_literal(
        &mut self,
        identifier: &'a str,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let expression_id = expression.id();
        match self.change_variable_literal(identifier, expression) {
            Some(new_expression_type) => {
                self.changes.insert(expression_id, new_expression_type);
            }
            _ => {}
        }
        Ok(())
    }

    fn pass_variable_assignment(
        &mut self,
        identifier: &'a str,
        value: &'a Expression<'a>,
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let expression_id = expression.id();
        if let Some(scope) = self.locals.get(&expression_id).cloned() {
            if scope > 0 && scope < self.scopes.len() - 1 {
                if self.class_scope.unwrap_or(0) == scope {
                    self.changes.insert(
                        expression_id,
                        ExpressionType::Set {
                            callee: Box::new(self.expression_factory.new_expression(
                                ExpressionType::VariableLiteral {
                                    identifier: "this",
                                },
                                expression.location.clone()
                            )),
                            property: identifier,
                            value: Box::new(value.clone()),
                        }
                    );
                } else {
                    self.missed_locals.last_mut().unwrap().insert(identifier);
                }
            }
        }
        self.pass_expression(value)?;
        Ok(())
    }

    fn pass_anonymous_function(
        &mut self,
        arguments: &'a [&'a str],
        body: &'a [Statement<'a>],
        expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        let new_function_name = self.create_new_function(
            Some(arguments),
            body.iter().cloned().map(Box::new).collect()
        )?;
        self.changes.insert(expression.id(), ExpressionType::VariableLiteral {
            identifier: new_function_name,
        });
        Ok(())
    }
}
