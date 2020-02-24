use crate::function::LoxFunction;
use crate::class::{LoxObject, LoxClass};
use crate::state::State;
use crate::value::{Value, ValueError, LoxModule};
use parser::types::{Expression, ExpressionType, FunctionHeader, ProgramError, SourceCodeLocation, Statement, StatementType, TokenType, Type};
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::convert::{TryInto, TryFrom};
use std::iter::FromIterator;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use parser::lexer::Lexer;
use parser::parser::Parser;

pub type EvaluationResult<'a> = Result<(State<'a>, Value<'a>), ProgramError<'a>>;

fn operation<'a, R, T: TryFrom<Value<'a>, Error=ValueError>>(l: Value<'a>, r: Value<'a>, op: fn(T, T) -> R) -> Result<R, ValueError> {
    let l_number = l.try_into()?;
    let r_number = r.try_into()?;
    Ok(op(l_number, r_number))
}

fn f32_math_operation<'a>(l: Value<'a>, r: Value<'a>, op: fn(f32, f32) -> f32) -> Result<Value<'a>, ValueError> {
    Ok(Value::Float {
        value: operation(l, r, op)?,
    })
}

fn i64_math_operation<'a>(l: Value<'a>, r: Value<'a>, op: fn(i64, i64) -> i64) -> Result<Value<'a>, ValueError> {
    Ok(Value::Integer {
        value: operation(l, r, op)?,
    })
}

fn math_operation<'a>(l: Value<'a>, r: Value<'a>, i64_op: fn(i64, i64) -> i64, f32_op: fn(f32, f32) -> f32) -> Result<Value<'a>, ValueError> {
    match (&l, &r) {
        (Value::Float { .. }, Value::Float { .. }) => f32_math_operation(l, r, f32_op),
        (Value::Float { .. }, Value::Integer { value }) => f32_math_operation(l, Value::Float { value: *value as _ }, f32_op),
        (Value::Integer { value }, Value::Float { .. }) => f32_math_operation(Value::Float { value: *value as _ }, r, f32_op),
        (Value::Integer { .. }, Value::Integer { .. }) => i64_math_operation(l, r, i64_op),
        _ => Err(ValueError::ExpectingNumber),

    }
}

fn comparison_operation<'a>(l: Value<'a>, r: Value<'a>, op: fn(f32, f32) -> bool) -> Result<Value<'a>, ValueError> {
    Ok(Value::Boolean {
        value: operation(l, r, op)?,
    })
}

fn statements_to_hash_set<'a>(statements: &[&Statement<'a>]) -> HashSet<FunctionHeader<'a>> {
    let mut map = HashSet::new();
    for s in statements {
        if let StatementType::FunctionDeclaration {
            name, arguments, ..
        } = &s.statement_type
        {
            map.insert(FunctionHeader {
                name: name.clone(),
                arity: arguments.len(),
            });
        } else {
            panic!("Unexpected statement");
        }
    }
    map
}

fn check_trait_methods<'a>(
    impl_methods: &[&Statement<'a>],
    trait_methods: &[FunctionHeader],
    location: &SourceCodeLocation<'a>,
) -> Result<(), Vec<ProgramError<'a>>> {
    let trait_methods_hash = HashSet::from_iter(trait_methods.iter().cloned());
    let impl_methods_hash = statements_to_hash_set(impl_methods);
    let missed_methods = trait_methods_hash.difference(&impl_methods_hash);
    let extra_methods = impl_methods_hash.difference(&trait_methods_hash);
    let mut errors = vec![];
    for missed_method in missed_methods {
        errors.push(ProgramError {
            location: location.clone(),
            message: format!(
                "Missing method {} of arity {}, in trait implementation",
                missed_method.name, missed_method.arity
            ),
        });
    }
    for extra_method in extra_methods {
        errors.push(ProgramError {
            location: location.clone(),
            message: format!(
                "Method {} of arity {} is not in trait declaration",
                extra_method.name, extra_method.arity
            ),
        });
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn is_trait(trait_name: &str, obj: &LoxObject) -> bool {
    if obj.traits.contains(trait_name) {
        true
    } else {
        if let Some(superclass) = &obj.superclass {
            is_trait(trait_name, superclass)
        } else {
            false
        }
    }
}

fn is_class<'a>(lox_class: &LoxClass<'a>, obj: &LoxObject<'a>) -> bool {
    if lox_class.name == obj.class_name {
        true
    } else {
        if let Some(superclass) = &obj.superclass {
            is_class(lox_class, superclass)
        } else {
            false
        }
    }
}

pub struct Interpreter<'a> {
    blacklist: RefCell<Vec<&'a str>>,
    pub locals: HashMap<usize, usize>,
    modules: Cell<HashMap<&'a str, Vec<Box<Statement<'a>>>>>,
    module_contents: Cell<HashMap<&'a str, String>>,
    paths: &'a [String],
}

impl<'a> Interpreter<'a> {
    pub fn new(paths: &'a [String], file: &'a str,) -> Interpreter<'a> {
        Interpreter {
            blacklist: RefCell::new(vec![file]),
            locals: HashMap::default(),
            modules: Cell::new(HashMap::default()),
            module_contents: Cell::new(HashMap::default()),
            paths,
        }
    }

    pub fn run(&'a self, content: &'a [Statement<'a>]) -> Result<State<'a>, ProgramError<'a>> {
        let mut current_state = State::default();
        for s in content {
            match self.evaluate(current_state, s) {
                Ok((next_state, _)) => {
                    current_state = next_state;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(current_state)
    }

    pub fn evaluate_expression(&'a self, state: State<'a>, expression: &'a Expression<'a>) -> EvaluationResult<'a> {
        match &expression.expression_type {
            ExpressionType::IsType {
                value, checked_type
            } => {
                let (s, value) = self.evaluate_expression(state, value)?;
                self.is_value_type(&value, checked_type, s, &expression.location)
            }
            ExpressionType::ModuleLiteral {
                module,
                field,
            } => {
                let value = self.look_up_variable(expression.id(), module, &state)
                    .ok_or_else(|| {
                        expression.create_program_error(&format!("Module `{}` not found!", module))
                    })?
                    .clone();
                if let Value::Module(LoxModule {
                    state: module_state,
                }) = value {
                    let (s, v) = self.evaluate_expression(module_state, field)?;
                    state.assign_at(self.locals[&expression.id()], module, &Value::Module(LoxModule {
                        state: s,
                    }));
                    Ok((state, v))
                } else {
                    Err(expression.create_program_error(&format!("Variable `{}` is not a module", module)))
                }
            }
            ExpressionType::ArrayElementSet {
                array,
                index,
                value,
            } => self.array_element_expression_set(array, index, value, state),
            ExpressionType::ArrayElement { array, index } => {
                self.array_element_expression(array, index, state)
            }
            ExpressionType::RepeatedElementArray { element, length } => {
                let (ns, element) = self.evaluate_expression(state, element)?;
                let (ns, length) = self.evaluate_expression(ns, length)?;
                if let Value::Integer { value: length } = length {
                    let elements = Rc::new(RefCell::new(vec![Box::new(element); length as _]));
                    Ok((
                        ns,
                        Value::Array {
                            elements,
                            capacity: length as _,
                        },
                    ))
                } else {
                    Err(expression.create_program_error("Array length should be an integer"))
                }
            }
            ExpressionType::Array { elements } => {
                let (next_state, elements) =
                    elements
                        .iter()
                        .try_fold((state, vec![]), |(s, mut elements), e| {
                            let (ns, e) = self.evaluate_expression(s, e)?;
                            elements.push(Box::new(e));
                            Ok((ns, elements))
                        })?;
                let elements = Rc::new(RefCell::new(elements));
                Ok((
                    next_state,
                    Value::Array {
                        capacity: elements.clone().borrow().len(),
                        elements: elements.clone(),
                    },
                ))
            }
            ExpressionType::Set {
                callee,
                property,
                value,
            } => self.set_property(state, callee, property, value),
            ExpressionType::Get { callee, property } => {
                self.get_property(state, callee, property)
            }
            ExpressionType::ExpressionLiteral { value } => Ok((state, value.into())),
            ExpressionType::VariableLiteral { identifier } => {
                let value = self.look_up_variable(expression.id(), identifier, &state)
                    .ok_or_else(|| {
                        expression.create_program_error(&format!("Variable `{}` not found!", identifier))
                    })?
                    .clone();
                Ok((state, value))
            }
            ExpressionType::Grouping { expression } => self.evaluate_expression(state, expression),
            ExpressionType::Unary {
                operand,
                operator: TokenType::Minus,
            } => {
                let (s, v) = self.evaluate_expression(state, operand)?;
                if v.is_number() {
                    Ok((s, -v))
                } else {
                    Err(expression.create_program_error("Can only negate numbers"))
                }
            }
            ExpressionType::Unary {
                operand,
                operator: TokenType::Bang,
            } => self.evaluate_expression(state, operand).map(|(s, v)| (s, !v)),
            ExpressionType::Unary { .. } => {
                Err(expression.create_program_error("Invalid unary operator"))
            }
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Plus,
            } => self.add_expressions(state, left, right, &expression.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Minus,
            } => self.value_math_operation(state, left, right, &expression.location, i64::sub, f32::sub),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Slash,
            } => self.div_expressions(state, left, right, &expression.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Star,
            } => self.value_math_operation(state, left, right, &expression.location, i64::mul, f32::mul),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Greater,
            } => self.value_comparison_operation(
                state,
                left,
                right,
                &expression.location,
                |f1, f2| f32::gt(&f1, &f2),
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::GreaterEqual,
            } => self.value_comparison_operation(
                state,
                left,
                right,
                &expression.location,
                |f1, f2| f32::ge(&f1, &f2),
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Less,
            } => self.value_comparison_operation(
                state,
                left,
                right,
                &expression.location,
                |f1, f2| f32::lt(&f1, &f2),
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::LessEqual,
            } => self.value_comparison_operation(
                state,
                left,
                right,
                &expression.location,
                |f1, f2| f32::le(&f1, &f2),
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::EqualEqual,
            } => self.eq_expressions(state, left, right),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::BangEqual,
            } => self.eq_expressions(state, left, right).map(|(s, v)| (s, !v)),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::And,
            } => self.boolean_expression(
                state,
                left,
                right,
                |left_value, right_value| {
                    if left_value.is_truthy() {
                        right_value
                    } else {
                        left_value
                    }
                },
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Or,
            } => self.boolean_expression(
                state,
                left,
                right,
                |left_value, right_value| {
                    if left_value.is_truthy() {
                        left_value
                    } else {
                        right_value
                    }
                },
            ),
            ExpressionType::Binary { .. } => {
                Err(expression.create_program_error("Invalid binary operator"))
            }
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => self.conditional_expression(state, condition, then_branch, else_branch),
            ExpressionType::VariableAssignment {
                expression: value,
                identifier,
            } => self.variable_assignment(
                state,
                identifier,
                expression.id(),
                value,
                &expression.location,
            ),
            ExpressionType::Call { callee, arguments } => {
                self.call_expression(state, callee, arguments)
            }
            ExpressionType::AnonymousFunction { arguments, body } => {
                let f = Value::Function(LoxFunction {
                    arguments: arguments.to_vec(),
                    body: body.iter().collect(),
                    environments: state.get_environments(),
                    location: expression.location.clone(),
                });
                Ok((state, f))
            }
        }
    }
    pub(crate) fn evaluate(
        &'a self,
        mut state: State<'a>,
        statement: &'a Statement<'a>,
    ) -> EvaluationResult<'a> {
        let state = match &statement.statement_type {
            StatementType::EOF => state,
            StatementType::Module {
                name, statements
            } => {
                unsafe { self.modules.as_ptr().as_mut() }.unwrap().insert(*name, statements.clone());
                self.process_module(state, name)?
            },
            StatementType::Import { name, } => {
                let statements = self.resolve_import(name, &statement.location)?
                    .into_iter()
                    .map(Box::new)
                    .collect();
                unsafe { self.modules.as_ptr().as_mut() }.unwrap().insert(*name, statements);
                self.process_module(state, name)?
            }
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                let (s, cond_value) = self.evaluate_expression(state, condition)?;
                if cond_value.is_truthy() {
                    self.evaluate(s, then)?.0
                } else if let Some(o) = otherwise {
                    self.evaluate(s, o)?.0
                } else {
                    s
                }
            }
            StatementType::Expression { expression } => self.evaluate_expression(state, expression)?.0,
            StatementType::Block { body } => {
                state.push();
                for st in body {
                    let (s, _) = self.evaluate(state, st)?;
                    state = s;
                    if state.broke_loop {
                        break;
                    }
                }
                state.pop();
                state
            }
            StatementType::VariableDeclaration { expression, name } => {
                let (mut s, v) = if let Some(e) = expression {
                    self.evaluate_expression(state, e)?
                } else {
                    (state, Value::Uninitialized)
                };
                s.insert_top(name.clone(), v);
                s
            }
            StatementType::PrintStatement { expression } => {
                let (s, v) = self.evaluate_expression(state, expression)?;
                println!("{}", v);
                s
            }
            StatementType::TraitDeclaration {
                name,
                methods,
                static_methods,
                setters,
                getters,
            } => {
                state.insert_top(
                    name.clone(),
                    Value::Trait {
                        name,
                        methods: methods.clone(),
                        static_methods: static_methods.clone(),
                        setters: setters.clone(),
                        getters: getters.clone(),
                    },
                );
                state
            }
            StatementType::TraitImplementation {
                class_name,
                getters,
                methods,
                setters,
                static_methods,
                trait_name,
            } => {
                if let (state, Value::Class(class)) = self.evaluate_expression(state, class_name)? {
                    if let (
                        ns,
                        Value::Trait {
                            methods: trait_methods,
                            static_methods: trait_static_methods,
                            getters: trait_getters,
                            setters: trait_setters,
                            name,
                        },
                    ) = self.evaluate_expression(state, trait_name)?
                    {
                        if class.implements(&name) {
                            return Err(statement.create_program_error(
                                format!("{} already implements {}", class.name, name).as_str(),
                            ));
                        } else {
                            class.append_trait(name);
                        }
                        let methods = &methods.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let static_methods = &static_methods
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&_>>();
                        let getters = &getters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let setters = &setters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let envs = ns.get_environments();
                        check_trait_methods(methods, &trait_methods, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(methods, envs.clone());
                        check_trait_methods(static_methods, &trait_static_methods, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_static_methods(static_methods, envs.clone());
                        check_trait_methods(getters, &trait_getters, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(getters, envs.clone());
                        check_trait_methods(setters, &trait_setters, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(setters, envs.clone());
                        ns
                    } else {
                        return Err(class_name
                            .create_program_error("You can implement traits only on classes"));
                    }
                } else {
                    return Err(
                        class_name.create_program_error("You can implement traits only on classes")
                    );
                }
            }
            StatementType::ClassDeclaration {
                getters,
                name,
                methods,
                setters,
                static_methods,
                superclass,
            } => {
                let (mut state, superclass) = if let Some(e) = superclass {
                    let (s, superclass) = self.evaluate_expression(state, e)?;
                    if let Value::Class(c) = superclass {
                        (s, Some(c))
                    } else {
                        return Err(statement.create_program_error("Superclass must be a class"));
                    }
                } else {
                    (state, None)
                };
                state.insert_top(
                    name.to_owned(),
                    Value::Class(LoxClass::new(
                        name.to_owned(),
                        &static_methods
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&Statement>>(),
                        &methods
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&Statement>>(),
                        &getters
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&Statement>>(),
                        &setters
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&Statement>>(),
                        superclass,
                        state.get_environments(),
                    )),
                );
                state
            }
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            } => {
                state.insert(
                    name.clone(),
                    Value::Function(LoxFunction {
                        arguments: arguments.clone(),
                        environments: state.get_environments(),
                        body: body.into_iter().map(AsRef::as_ref).collect(),
                        location: statement.location.clone(),
                    }),
                );
                state
            }
            StatementType::Return { value } if state.in_function => match value {
                None => state,
                Some(e) => {
                    let (mut s, v) = self.evaluate_expression(state, e)?;
                    s.add_return_value(v);
                    s
                }
            },
            StatementType::Return { .. } =>
                return Err(statement.create_program_error("Return outside function")),
            StatementType::While { condition, action } => {
                state.loop_count += 1;
                while {
                    let (s, v) = self.evaluate_expression(state, condition)?;
                    state = s;
                    state.loop_count > 0 && v.is_truthy()
                } {
                    let (s, _) = self.evaluate(state, action)?;
                    state = s;
                    if state.broke_loop {
                        break;
                    }
                }
                state.loop_count -= 1;
                state.broke_loop = false;
                state
            }
            StatementType::Break if state.loop_count > 0 => {
                state.broke_loop = true;
                state
            }
            StatementType::Break =>
                return Err(statement.create_program_error("Break outside loop")),
        };
        Ok((state, Value::Nil))
    }

    fn get_module_content(&'a self, name: &'a str) -> &'a str {
        unsafe { self.module_contents.as_ptr().as_ref() }.unwrap().get(name).unwrap()
    }

    fn get_module_statements(
        &'a self,
        name: &'a str,
    ) -> &'a [Box<Statement<'a>>] {
        unsafe { self.modules.as_ptr().as_ref() }.unwrap().get(name).unwrap()
    }

    fn open_import(
        &'a self,
        name: &'a str,
        location: &SourceCodeLocation<'a>,
    ) -> Result<String, ProgramError<'a>> {
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

    fn resolve_import(
        &'a self,
        name: &'a str,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Vec<Statement<'a>>, ProgramError<'a>> {
        if self.blacklist.borrow().contains(&name) {
            return Err(ProgramError {
                message: format!("Circular import of {}", name),
                location: location.clone(),
            });
        }
        let content = self.open_import(name, location)?;
        unsafe { self.module_contents.as_ptr().as_mut() }.unwrap()
            .insert(name, content);
        let mut lexer = Lexer::new(self.get_module_content(name), name);
        lexer.parse()
            .and_then(|tt| {
                let parser = Parser::new(tt.into_iter().peekable());
                parser.parse()
            })
            .map_err(|ee| ee[0].clone())
    }

    fn process_module<'b>(
        &'a self,
        mut state: State<'a>,
        name: &'a str,
    ) -> Result<State<'a>, ProgramError<'a>> {
        let statements = self.get_module_statements(name);
        self.blacklist.borrow_mut().push(name);
        let module_state: State = statements.iter()
            .try_fold(State::default(), |state, statement| {
                Ok(self.evaluate(state, statement)?.0)
            })?;
        self.blacklist.borrow_mut().pop();
        state.insert_top(name, Value::Module(LoxModule {
            state: module_state,
        }));
        Ok(state)
    }

    fn variable_assignment(
        &'a self,
        state: State<'a>,
        name: &'a str,
        id: usize,
        expression: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        match self.locals.get(&id) {
            Some(env) => {
                let (s, value) = self.evaluate_expression(state, expression)?;
                s.assign_at(*env, name, &value);
                Ok((s, value))
            }
            None => Err(ProgramError {
                location: location.clone(),
                message: format!("Variable `{}` not found!", name),
            }),
        }
    }

    fn call_expression(
        &'a self,
        state: State<'a>,
        callee: &'a Expression<'a>,
        arguments: &'a [Box<Expression<'a>>],
    ) -> EvaluationResult<'a> {
        let (next_state, function_value) = self.evaluate_expression(state, callee)?;
        match function_value {
            Value::Class(c) => {
                let instance = LoxObject::new(c);
                let mut values = vec![];
                let mut current_state = next_state;
                for e in arguments {
                    let (value_status, value) = self.evaluate_expression(current_state, e)?;
                    current_state = value_status;
                    values.push(value);
                }
                instance.init(&values, &self, &callee.location)?;
                Ok((current_state, Value::Object(instance)))
            }
            Value::Function(f) if f.arguments.len() != arguments.len() => Err(callee
                .create_program_error(
                    format!(
                        "Wrong number of arguments! Expected: {} Got: {}",
                        f.arguments.len(),
                        arguments.len()
                    )
                        .as_str(),
                )),
            Value::Function(f) => {
                let mut values = vec![];
                let mut current_state = next_state;
                for e in arguments {
                    let (value_status, value) = self.evaluate_expression(current_state, e)?;
                    current_state = value_status;
                    values.push(value);
                }
                f.eval(&values, &self).map(|v| (current_state, v))
            }
            _ => Err(callee.create_program_error("Only functions or classes can be called!")),
        }
    }

    fn conditional_expression(
        &'a self,
        state: State<'a>,
        condition: &'a Expression<'a>,
        then_branch: &'a Expression<'a>,
        else_branch: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let (s, condition) = self.evaluate_expression(state, condition)?;
        self.evaluate_expression(s,
             if condition.is_truthy() {
                 then_branch
             } else {
                 else_branch
             }
        )
    }

    fn boolean_expression(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        op: fn(Value<'a>, Value<'a>) -> Value<'a>,
    ) -> EvaluationResult<'a> {
        let (s, left_value) = self.evaluate_expression(state, left)?;
        let (final_state, right_value) = self.evaluate_expression(s, right)?;
        Ok((final_state, op(left_value, right_value)))
    }

    fn value_comparison_operation(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
        op: fn(f32, f32) -> bool,
    ) -> EvaluationResult<'a> {
        let (s, left_value) = self.evaluate_expression(state, left)?;
        let (final_state, right_value) = self.evaluate_expression(s, right)?;
        comparison_operation(left_value, right_value, op)
            .map(|v| (final_state, v))
            .map_err(|e| e.into_program_error(location))
    }

    fn eq_expressions(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let (next_state, left_value) = self.evaluate_expression(state, left)?;
        let (final_state, right_value) = self.evaluate_expression(next_state, right)?;
        Ok((
            final_state,
            Value::Boolean {
                value: left_value == right_value,
            },
        ))
    }

    fn value_math_operation(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
        i64_op: fn(i64, i64) -> i64,
        f32_op: fn(f32, f32) -> f32,
    ) -> EvaluationResult<'a> {
        let (s, left_value) = self.evaluate_expression(state, left)?;
        let (final_state, right_value) = self.evaluate_expression(s, right)?;
        math_operation(left_value, right_value, i64_op, f32_op)
            .map(|v| (final_state, v))
            .map_err(|e| e.into_program_error(location))
    }

    fn div_expressions(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        let (s, left_value) = self.evaluate_expression(state, left)?;
        let (final_state, right_value) = self.evaluate_expression(s, right)?;
        match right_value {
            Value::Float { value } if value == 0f32 => {
                Err(right.create_program_error("Division by zero!"))
            }
            Value::Integer { value } if value == 0 => {
                Err(right.create_program_error("Division by zero!"))
            }
            _ => Ok(()),
        }?;
        math_operation(left_value, right_value, i64::div, f32::div)
            .map(|v| (final_state, v))
            .map_err(|e| e.into_program_error(&location))
    }

    fn add_expressions(
        &'a self,
        state: State<'a>,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        let (s, left_value) = self.evaluate_expression(state, left)?;
        if left_value.is_number() {
            let (final_state, right_value) = self.evaluate_expression(s, right)?;
            math_operation(left_value, right_value, i64::add, f32::add)
                .map(|v| (final_state, v))
                .map_err(|e| e.into_program_error(location))
        } else {
            let left_string: String = left_value
                .try_into()
                .map_err(|e: ValueError| e.into_program_error(location))?;
            let (final_state, right_value) = self.evaluate_expression(s, right)?;
            let right_string: String = right_value
                .try_into()
                .map_err(|e: ValueError| e.into_program_error(location))?;
            Ok((
                final_state,
                Value::String {
                    value: format!("{}{}", left_string, right_string),
                },
            ))
        }
    }

    fn get_property(
        &'a self,
        state: State<'a>,
        callee: &'a Expression<'a>,
        property: &'a str,
    ) -> EvaluationResult<'a> {
        let (next_state, object) = self.evaluate_expression(state, callee)?;
        match object {
            Value::Object(instance) => {
                if let Some(v) = instance.get(property) {
                    Ok((next_state, v))
                } else {
                    if let Some(v) = instance.get_getter(property) {
                        v.eval(&[], &self).map(|v| (next_state, v))
                    } else {
                        Err(callee
                            .create_program_error(format!("Undefined property {}.", property).as_str()))
                    }
                }
            }
            Value::Class(c) => {
                if let Some(v) = c.static_instance.get(property) {
                    Ok((next_state, v))
                } else {
                    Err(callee
                        .create_program_error(format!("Undefined property {}.", property).as_str()))
                }
            }
            _ => Err(callee.create_program_error("Only instances have properties")),
        }
    }

    fn set_property(
        &'a self,
        state: State<'a>,
        callee: &'a Expression<'a>,
        property: &'a str,
        value: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let (ts, object) = self.evaluate_expression(state, callee)?;
        if let Value::Object(mut instance) = object {
            let (final_state, value) = self.evaluate_expression(ts, value)?;
            if let Some(f) = instance.get_setter(property) {
                f.eval(&[value], &self).map(|v| (final_state, v))
            } else {
                instance.set(property.clone(), value.clone());
                Ok((final_state, value))
            }
        } else {
            Err(callee.create_program_error("Only instances have properties"))
        }
    }

    fn array_element_expression_set(
        &'a self,
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        value: &'a Expression<'a>,
        state: State<'a>,
    ) -> EvaluationResult<'a> {
        self.array_element_operation(
            array, index, state, |ns, elements, index_value| {
                let (ns, value) = self.evaluate_expression(ns, value)?;
                elements.borrow_mut()[index_value] = Box::new(value.clone());
                Ok((ns, value))
            }
        )
    }

    fn array_element_expression(
        &'a self,
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        state: State<'a>,
    ) -> EvaluationResult<'a> {
        self.array_element_operation(
            array, index, state, |ns, elements, index_value| {
                Ok((ns, *elements.borrow()[index_value].clone()))
            }
        )
    }

    fn array_element_operation<I: Fn(State<'a>, Rc<RefCell<Vec<Box<Value<'a>>>>>, usize) -> EvaluationResult<'a>>(
        &'a self,
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        state: State<'a>,
        op: I,
    ) -> EvaluationResult<'a> {
        let (ns, array_value) = self.evaluate_expression(state, array)?;
        if let Value::Array { elements, capacity } = array_value {
            let (ns, index_value) = self.evaluate_expression(ns, index)?;
            let index_value: i64 = i64::try_from(index_value).map_err(|e: ValueError| index.create_program_error(
                e.to_string().as_str(),
            ))?;
            if (index_value as usize) < capacity {
                op(ns, elements, index_value as usize)
            } else {
                Err(index.create_program_error(
                    format!(
                        "You can't access element {} in an array of {} elements",
                        index_value, capacity
                    )
                        .as_str(),
                ))
            }
        } else {
            Err(array.create_program_error("You can only index arrays"))
        }
    }

    fn look_up_variable(
        &'a self,
        expression_id: usize,
        name: &str,
        state: &State<'a>,
    ) -> Option<Value<'a>> {
        if let Some(env) = self.locals.get(&expression_id) {
            state.get_at(name, *env)
        } else {
            state.get_global(name)
        }
    }

    fn is_value_type(
        &'a self,
        value: &Value<'a>,
        checked_type: &'a Type<'a>,
        state: State<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        match (value, checked_type) {
            (Value::Nil, Type::Nil) => Ok((state, Value::Boolean { value: true })),
            (Value::Nil, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Boolean { .. }, Type::Boolean) => Ok((state, Value::Boolean { value: true })),
            (Value::Boolean { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Integer { .. }, Type::Integer) => Ok((state, Value::Boolean { value: true })),
            (Value::Integer { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Float { .. }, Type::Float) => Ok((state, Value::Boolean { value: true })),
            (Value::Float { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Module { .. }, Type::Module) => Ok((state, Value::Boolean { value: true })),
            (Value::Module { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::String { .. }, Type::String) => Ok((state, Value::Boolean { value: true })),
            (Value::String { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Array { .. }, Type::Array) => Ok((state, Value::Boolean { value: true })),
            (Value::Array { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Function(_), Type::Function) => Ok((state, Value::Boolean { value: true })),
            (Value::Function(_), _) => Ok((state, Value::Boolean { value: false })),
            (Value::Trait { .. }, Type::Trait) => Ok((state, Value::Boolean { value: true })),
            (Value::Trait { .. }, _) => Ok((state, Value::Boolean { value: false })),
            (Value::Class(_), Type::Class) => Ok((state, Value::Boolean { value: true })),
            (Value::Class(_), _) => Ok((state, Value::Boolean { value: false })),
            (Value::Object(obj), Type::UserDefined(c)) => {
                let (s, v) = self.evaluate_expression(state, c)?;
                match v {
                    Value::Class(lox_class) => {
                        Ok((s, Value::Boolean { value: is_class(&lox_class, obj) }))
                    }
                    Value::Trait { name, .. } => {
                        Ok((s, Value::Boolean { value: is_trait(&name, obj) }))
                    }
                    _ => Err(ProgramError {
                        location: c.location.clone(),
                        message: "Objects can only be either an implementation of a class or an object".to_owned(),
                    })
                }
            },
            (Value::Object(_), _) => Ok((state, Value::Boolean { value: false })),
            _ => Err(ProgramError {
                location: location.clone(),
                message: "Invalid value to check for type".to_owned(),
            })
        }
    }
}

#[cfg(test)]
mod common_test {
    use parser::types::{ExpressionType, SourceCodeLocation, Expression, ExpressionFactory};

    pub fn create_expression<'a>(
        expression_type: ExpressionType<'a>,
        location: SourceCodeLocation<'a>,
    ) -> Expression<'a> {
        let mut factory = ExpressionFactory::new();
        factory.new_expression(expression_type, location)
    }

    pub fn get_variable<'a>(s: &'a str, location: &SourceCodeLocation<'a>) -> Expression<'a> {
        create_expression(
            ExpressionType::VariableLiteral {
                identifier: s,
            },
            location.clone(),
        )
    }
}


#[cfg(test)]
mod test_statement {
    use crate::function::LoxFunction;
    use crate::state::State;
    use crate::value::Value;
    use parser::types::{
        Expression, ExpressionType, Literal, ProgramError, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use std::collections::HashMap;
    use super::common_test::{create_expression, get_variable};
    use crate::interpreter::Interpreter;

    #[test]
    fn test_if_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let statement = Statement {
            statement_type: StatementType::If {
                condition: create_expression_number(1.0, &location),
                then: Box::new(create_variable_assignment_statement(
                    "identifier",
                    1.0,
                    &location,
                )),
                otherwise: Some(Box::new(create_variable_assignment_statement(
                    "identifier",
                    0.0,
                    &location,
                ))),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 2.0 });
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 1.0 }));
    }

    #[test]
    fn test_if_statement_else() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let statement = Statement {
            statement_type: StatementType::If {
                condition: create_expression_number(0.0, &location),
                then: Box::new(create_variable_assignment_statement(
                    "identifier",
                    1.0,
                    &location,
                )),
                otherwise: Some(Box::new(create_variable_assignment_statement(
                    "identifier",
                    0.0,
                    &location,
                ))),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 2.0 });
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 0.0 }));
    }

    #[test]
    fn test_expression_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let statement = create_variable_assignment_statement("identifier", 0.0, &location);
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 2.0 });
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 0.0 }));
    }

    #[test]
    fn test_block_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let statement = Statement {
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(create_variable_assignment_statement(
                        "identifier",
                        0.0,
                        &location,
                    )),
                    Box::new(create_variable_assignment_statement(
                        "identifier1",
                        1.0,
                        &location,
                    )),
                    Box::new(create_variable_assignment_statement(
                        "identifier2",
                        2.0,
                        &location,
                    )),
                ],
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 2.0 });
        state.insert("identifier1", Value::Float { value: 2.0 });
        state.insert("identifier2", Value::Float { value: 0.0 });
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 0.0 }));
        assert_eq!(s.find("identifier1"), Some(Value::Float { value: 1.0 }));
        assert_eq!(s.find("identifier2"), Some(Value::Float { value: 2.0 }));
    }

    #[test]
    fn test_variable_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::VariableDeclaration {
                expression: Some(create_expression_number(1.0, &location)),
                name: "identifier",
            },
            location,
        };
        let state = State::default();
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 1.0 }));
    }

    #[test]
    fn test_function_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::FunctionDeclaration {
                name: "function",
                arguments: vec![],
                body: vec![Box::new(Statement {
                    statement_type: StatementType::EOF,
                    location: location.clone(),
                })],
            },
            location: location.clone(),
        };
        let state = State::default();
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        let value = s.find("function").unwrap();
        match value {
            Value::Function(LoxFunction {
                arguments,
                body,
                location: l,
                ..
            }) => {
                assert_eq!(arguments, Vec::<String>::new());
                assert_eq!(
                    body,
                    vec![&Statement {
                        statement_type: StatementType::EOF,
                        location: location.clone(),
                    }]
                );
                assert_eq!(l, location);
            }
            _ => panic!("Wrong type! Should be Function!"),
        }
    }

    #[test]
    fn test_return_in_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location,
        };
        let interpreter = Interpreter::new(&[], "");
        let mut state = State::default();
        state.in_function = true;
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.return_value, Some(Box::new(Value::Float { value: 1.0 })));
    }

    #[test]
    fn test_return_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location: location.clone(),
        };
        let state = State::default();
        let r = interpreter.evaluate(state, &statement);
        assert_eq!(
            r,
            Err(ProgramError {
                message: "Return outside function".to_owned(),
                location,
            })
        );
    }

    #[test]
    fn test_break_in_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location,
        };
        let interpreter = Interpreter::new(&[], "");
        let mut state = State::default();
        state.loop_count = 1;
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert!(s.broke_loop);
        assert_eq!(s.loop_count, 1);
    }

    #[test]
    fn test_break_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location: location.clone(),
        };
        let interpreter = Interpreter::new(&[], "");
        let state = State::default();
        let r = interpreter.evaluate(state, &statement);
        assert_eq!(
            r,
            Err(ProgramError {
                message: "Break outside loop".to_owned(),
                location,
            })
        );
    }

    #[test]
    fn test_while_loop() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let identifier_expression = get_variable("identifier", &location);
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let statement = Statement {
            statement_type: StatementType::While {
                condition: create_expression(
                    ExpressionType::Binary {
                        operator: TokenType::Less,
                        left: Box::new(identifier_expression.clone()),
                        right: Box::new(create_expression_number(10f32, &location)),
                    },
                    location.clone(),
                ),
                action: Box::new(Statement {
                    statement_type: StatementType::Expression {
                        expression: create_expression(
                            ExpressionType::VariableAssignment {
                                identifier: "identifier",
                                expression: Box::new(create_expression(
                                    ExpressionType::Binary {
                                        operator: TokenType::Plus,
                                        left: Box::new(identifier_expression.clone()),
                                        right: Box::new(create_expression_number(1.0, &location)),
                                    },
                                    location.clone(),
                                )),
                            },
                            location.clone(),
                        ),
                    },
                    location: location.clone(),
                }),
            },
            location,
        };
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 0.0 });
        let (s, _) = interpreter.evaluate(state, &statement).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Float { value: 10.0 }));
    }

    fn create_variable_assignment_statement<'a>(
        id: &'a str,
        value: f32,
        location: &SourceCodeLocation<'a>,
    ) -> Statement<'a> {
        Statement {
            statement_type: StatementType::Expression {
                expression: create_expression(
                    ExpressionType::VariableAssignment {
                        identifier: id,
                        expression: Box::new(create_expression_number(value, location)),
                    },
                    location.clone(),
                ),
            },
            location: location.clone(),
        }
    }

    fn create_expression_number<'a>(value: f32, location: &SourceCodeLocation<'a>) -> Expression<'a> {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::Float(value),
            },
            location.clone(),
        )
    }
}

#[cfg(test)]
mod test_expression {
    use crate::function::LoxFunction;
    use crate::interpreter::Interpreter;
    use crate::state::State;
    use crate::value::Value;
    use parser::types::{
        DataKeyword, Expression, ExpressionType, Literal, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use std::collections::HashMap;
    use super::common_test::{create_expression, get_variable};

    #[test]
    fn test_expression_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let state = State::default();
        let interpreter = Interpreter::new(&[], "");
        let e = get_number(1.0, &location);
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &e)
            .unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_variable_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = get_variable("variable", &location);
        let mut state = State::default();
        let interpreter = Interpreter::new(&[], "");
        state.insert("variable", Value::Float { value: 1.0 });
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_group_expression() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Grouping {
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let interpreter = Interpreter::new(&[], "");
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_minus_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Unary {
                operator: TokenType::Minus,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let interpreter = Interpreter::new(&[], "");
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: -1.0 });
    }

    #[test]
    fn test_bang_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Unary {
                operator: TokenType::Bang,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let interpreter = Interpreter::new(&[], "");
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: false });
    }

    #[test]
    fn test_sum() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 2.0 });
    }

    #[test]
    fn test_sum_strings() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_string("1", &location)),
                right: Box::new(get_string("2", &location)),
            },
            location,
        );
        let state = State::default();
        let interpreter = Interpreter::new(&[], "");
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(
            got,
            Value::String {
                value: "12".to_string()
            }
        );
    }

    #[test]
    fn test_sub() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Minus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let interpreter = Interpreter::new(&[], "");
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 0.0 });
    }

    #[test]
    fn test_mult() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Star,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 2.0 });
    }

    #[test]
    fn test_div() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Slash,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Greater,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal_with_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Less,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal_with_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::EqualEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_different() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::BangEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_and() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::And,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(true, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_or() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Or,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(false, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_conditional() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Conditional {
                condition: Box::new(get_boolean(true, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_conditional_else_branch() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let expression = create_expression(
            ExpressionType::Conditional {
                condition: Box::new(get_boolean(false, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 2.0 });
    }

    #[test]
    fn test_variable_assignment() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.locals = locals;
        let expression = create_expression(
            ExpressionType::VariableAssignment {
                identifier: "identifier",
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let mut state = State::default();
        state.insert("identifier", Value::Float { value: 0.0 });
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        state.insert("identifier", Value::Float { value: 1.0 });
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Float { value: 1.0 });
    }

    #[test]
    fn test_function_call() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let expression = create_expression(
            ExpressionType::Call {
                callee: Box::new(get_variable("function", &location)),
                arguments: vec![],
            },
            location.clone(),
        );
        let mut state = State::default();
        let interpreter = Interpreter::new(&[], "");
        let s = Statement {
            statement_type: StatementType::VariableDeclaration {
                expression: Some(get_number(1.0, &location)),
                name: "identifier",
            },
            location: location.clone(),
        };
        state.insert("identifier", Value::Float { value: 0.0 });
        state.insert(
            "function",
            Value::Function(LoxFunction {
                arguments: vec![],
                environments: state.get_environments(),
                body: vec![&s],
                location,
            }),
        );
        let (final_state, got) = interpreter.evaluate_expression(state.clone(), &expression).unwrap();
        state.last().borrow_mut().remove("function");
        final_state.last().borrow_mut().remove("function");
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Nil);
    }

    fn get_string<'a>(s: &'a str, location: &SourceCodeLocation<'a>) -> Expression<'a> {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::QuotedString(s),
            },
            location.clone(),
        )
    }

    fn get_number<'a>(n: f32, location: &SourceCodeLocation<'a>) -> Expression<'a> {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::Float(n),
            },
            location.clone(),
        )
    }

    fn get_boolean<'a>(n: bool, location: &SourceCodeLocation<'a>) -> Expression<'a> {
        let keyword = if n {
            DataKeyword::True
        } else {
            DataKeyword::False
        };
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::Keyword(keyword),
            },
            location.clone(),
        )
    }
}
