use ahash::{AHashMap as HashMap, AHashSet as HashSet};
use crate::function::LoxFunction;
use crate::class::{LoxObject, LoxClass};
use crate::state::State;
use crate::value::{Value, ValueError, LoxTrait, LoxArray};
use parser::types::{Expression, ExpressionType, FunctionHeader, ProgramError, SourceCodeLocation, Statement, StatementType, TokenType, Type};
use std::cell::{Cell, RefCell};
use std::convert::{TryInto, TryFrom};
use std::iter::FromIterator;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use parser::lexer::Lexer;
use parser::parser::Parser;

pub type EvaluationResult<'a> = Result<Value<'a>, ProgramError<'a>>;

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
                name,
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
    pub blacklist: RefCell<Vec<&'a str>>,
    pub locals: HashMap<usize, usize>,
    modules: Cell<HashMap<&'a str, Vec<Box<Statement<'a>>>>>,
    module_contents: Cell<HashMap<&'a str, String>>,
    module_interpreters: Cell<HashMap<&'a str, Box<Interpreter<'a>>>>,
    paths: &'a [String],
    pub state: RefCell<State<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new(paths: &'a [String], file: &'a str,) -> Interpreter<'a> {
        Interpreter {
            blacklist: RefCell::new(vec![file]),
            locals: HashMap::default(),
            modules: Cell::new(HashMap::default()),
            module_contents: Cell::new(HashMap::default()),
            module_interpreters: Cell::new(HashMap::default()),
            state: RefCell::new(State::default()),
            paths,
        }
    }

    pub fn run(&'a self, content: &'a [Statement<'a>]) -> Result<(), ProgramError<'a>> {
        for s in content {
            self.evaluate(s)?;
        }
        Ok(())
    }

    pub fn evaluate_expression(&'a self, expression: &'a Expression<'a>) -> EvaluationResult<'a> {
        match &expression.expression_type {
            ExpressionType::IsType {
                value, checked_type
            } => {
                let value = self.evaluate_expression(value)?;
                self.is_value_type(&value, checked_type, &expression.location)
            }
            ExpressionType::ModuleLiteral {
                module,
                field,
            } => {
                let value = self.look_up_variable(expression.id(), module)
                    .ok_or_else(|| {
                        expression.create_program_error(&format!("Module `{}` not found!", module))
                    })?;
                if let Value::Module(module) = value {
                    self.get_module_interpreter(module).evaluate_expression(field)
                } else {
                    Err(expression.create_program_error(&format!("Variable `{}` is not a module", module)))
                }
            }
            ExpressionType::ArrayElementSet {
                array,
                index,
                value,
            } => self.array_element_expression_set(array, index, value),
            ExpressionType::ArrayElement { array, index } => {
                self.array_element_expression(array, index)
            }
            ExpressionType::RepeatedElementArray { element, length } => {
                let element = self.evaluate_expression(element)?;
                let length = self.evaluate_expression(length)?;
                if let Value::Integer { value: length } = length {
                    let elements = vec![Box::new(element); length as _];
                    Ok(Value::Array(Rc::new(RefCell::new(LoxArray {
                        elements,
                        capacity: length as _,
                    }))))
                } else {
                    Err(expression.create_program_error("Array length should be an integer"))
                }
            }
            ExpressionType::Array { elements } => {
                let elements =
                    elements
                        .iter()
                        .try_fold(vec![], |mut elements, e| {
                            let e = self.evaluate_expression(e)?;
                            elements.push(Box::new(e));
                            Ok(elements)
                        })?;
                Ok(Value::Array(Rc::new(RefCell::new(LoxArray {
                    capacity: elements.len(),
                    elements,
                }))),
                )
            }
            ExpressionType::Set {
                callee,
                property,
                value,
            } => self.set_property(callee, property, value),
            ExpressionType::Get { callee, property } => {
                self.get_property(callee, property)
            }
            ExpressionType::ExpressionLiteral { value } => Ok(value.into()),
            ExpressionType::VariableLiteral { identifier } =>
                self.look_up_variable(expression.id(), identifier)
                    .ok_or_else(|| {
                        expression.create_program_error(&format!("Variable `{}` not found!", identifier))
                    }),
            ExpressionType::Grouping { expression } => self.evaluate_expression(expression),
            ExpressionType::Unary {
                operand,
                operator: TokenType::Minus,
            } => {
                let v = self.evaluate_expression(operand)?;
                if v.is_number() {
                    Ok(-v)
                } else {
                    Err(expression.create_program_error("Can only negate numbers"))
                }
            }
            ExpressionType::Unary {
                operand,
                operator: TokenType::Bang,
            } => self.evaluate_expression(operand).map(|v| !v),
            ExpressionType::Unary { .. } => {
                Err(expression.create_program_error("Invalid unary operator"))
            }
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Plus,
            } => self.add_expressions(left, right, &expression.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Minus,
            } => self.value_math_operation(left, right, &expression.location, i64::sub, f32::sub),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Slash,
            } => self.div_expressions(left, right, &expression.location),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Star,
            } => self.value_math_operation(left, right, &expression.location, i64::mul, f32::mul),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Greater,
            } => self.value_comparison_operation(
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
                left,
                right,
                &expression.location,
                |f1, f2| f32::le(&f1, &f2),
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::EqualEqual,
            } => self.eq_expressions(left, right),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::BangEqual,
            } => self.eq_expressions(left, right).map(|v| !v),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::And,
            } => self.boolean_expression(
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
            } => self.conditional_expression(condition, then_branch, else_branch),
            ExpressionType::VariableAssignment {
                expression: value,
                identifier,
            } => self.variable_assignment(
                identifier,
                expression.id(),
                value,
                &expression.location,
            ),
            ExpressionType::Call { callee, arguments } => {
                self.call_expression(callee, arguments)
            }
            ExpressionType::AnonymousFunction { arguments, body } => {
                let f = Value::Function(Rc::new(LoxFunction {
                    arguments: arguments.to_vec(),
                    body: body.iter().collect(),
                    environments: self.state.borrow().get_environments(),
                    location: expression.location.clone(),
                }));
                Ok(f)
            }
        }
    }
    pub(crate) fn evaluate(
        &'a self,
        statement: &'a Statement<'a>,
    ) -> EvaluationResult<'a> {
        match &statement.statement_type {
            StatementType::EOF => {},
            StatementType::Module {
                name, statements
            } => {
                unsafe { self.modules.as_ptr().as_mut() }.unwrap().insert(*name, statements.clone());
                self.process_module(name)?;
            },
            StatementType::Import { name, } => {
                let statements = self.resolve_import(name, &statement.location)?
                    .into_iter()
                    .map(Box::new)
                    .collect();
                unsafe { self.modules.as_ptr().as_mut() }.unwrap().insert(*name, statements);
                self.process_module(name)?;
            }
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                let cond_value = self.evaluate_expression(condition)?;
                if cond_value.is_truthy() {
                    self.evaluate(then)?;
                } else if let Some(o) = otherwise {
                    self.evaluate(o)?;
                }
            }
            StatementType::Expression { expression } => {
                self.evaluate_expression(expression)?;
            },
            StatementType::Block { body } => {
                self.state.borrow_mut().push();
                for st in body {
                    self.evaluate(st)?;
                    if self.state.borrow().broke_loop {
                        break;
                    }
                }
                self.state.borrow_mut().pop();
            }
            StatementType::VariableDeclaration { expression, name } => {
                let v = if let Some(e) = expression {
                    self.evaluate_expression(e)?
                } else {
                    Value::Uninitialized
                };
                self.state.borrow_mut().insert_top(name, v);
            }
            StatementType::PrintStatement { expression } => {
                let v = self.evaluate_expression(expression)?;
                println!("{}", v);
            }
            StatementType::TraitDeclaration {
                name,
                methods,
                static_methods,
                setters,
                getters,
            } => {
                self.state.borrow_mut().insert_top(
                    name,
                    Value::Trait(Rc::new(LoxTrait {
                        name,
                        methods: methods.clone(),
                        static_methods: static_methods.clone(),
                        setters: setters.clone(),
                        getters: getters.clone(),
                    })),
                );
            }
            StatementType::TraitImplementation {
                class_name,
                getters,
                methods,
                setters,
                static_methods,
                trait_name,
            } => {
                if let Value::Class(class) = self.evaluate_expression(class_name)? {
                    if let Value::Trait(t) = self.evaluate_expression(trait_name)?
                    {
                        if class.implements(t.name) {
                            return Err(statement.create_program_error(
                                format!("{} already implements {}", class.name, t.name).as_str(),
                            ));
                        } else {
                            class.append_trait(t.name);
                        }
                        let methods = &methods.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let static_methods = &static_methods
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&_>>();
                        let getters = &getters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let setters = &setters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let envs = self.state.borrow().get_environments();
                        check_trait_methods(methods, &t.methods, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(methods, envs.clone());
                        check_trait_methods(static_methods, &t.static_methods, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_static_methods(static_methods, envs.clone());
                        check_trait_methods(getters, &t.getters, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_getters(getters, envs.clone());
                        check_trait_methods(setters, &t.setters, &statement.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_setters(setters, envs.clone());
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
                let superclass = if let Some(e) = superclass {
                    let superclass = self.evaluate_expression(e)?;
                    if let Value::Class(c) = superclass {
                        Some(c)
                    } else {
                        return Err(statement.create_program_error("Superclass must be a class"));
                    }
                } else {
                    None
                };
                let environments = self.state.borrow().get_environments();
                self.state.borrow_mut().insert_top(
                    name.to_owned(),
                    Value::Class(Rc::new(LoxClass::new(
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
                        environments,
                    ))),
                );
            }
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
                ..
            } => {
                let environments = self.state.borrow().get_environments();
                self.state.borrow_mut().insert(
                    name,
                    Value::Function(Rc::new(LoxFunction {
                        arguments: arguments.clone(),
                        body: body.into_iter().map(AsRef::as_ref).collect(),
                        location: statement.location.clone(),
                        environments,
                    })),
                );
            }
            StatementType::Return { value } if self.state.borrow().in_function => match value {
                None => {},
                Some(e) => {
                    let v = self.evaluate_expression(e)?;
                    self.state.borrow_mut().add_return_value(v);
                }
            },
            StatementType::Return { .. } =>
                return Err(statement.create_program_error("Return outside function")),
            StatementType::While { condition, action } => {
                self.state.borrow_mut().loop_count += 1;
                while {
                    let v = self.evaluate_expression(condition)?;
                    self.state.borrow().loop_count > 0 && v.is_truthy()
                } {
                    self.evaluate(action)?;
                    if self.state.borrow().broke_loop {
                        break;
                    }
                }
                self.state.borrow_mut().loop_count -= 1;
                self.state.borrow_mut().broke_loop = false;
            }
            StatementType::Break if self.state.borrow().loop_count > 0 => {
                self.state.borrow_mut().broke_loop = true;
            }
            StatementType::Break =>
                return Err(statement.create_program_error("Break outside loop")),
        };
        Ok(Value::Nil)
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


    fn get_module_interpreter(
        &'a self,
        name: &'a str,
    ) -> &'a Box<Interpreter<'a>> {
        unsafe { self.module_interpreters.as_ptr().as_ref() }.unwrap().get(name).unwrap()
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
                parser.parse().map(|t| t)
            })
            .map_err(|ee| ee[0].clone())
    }

    fn process_module<'b>(
        &'a self,
        name: &'a str,
    ) -> Result<(), ProgramError<'a>> {
        let statements = self.get_module_statements(name);
        let mut interpreter = Interpreter::new(&self.paths, name);
        interpreter.locals = self.locals.clone();
        interpreter.blacklist.borrow_mut().extend(&*self.blacklist.borrow());
        unsafe { self.module_interpreters.as_ptr().as_mut() }.unwrap().insert(name, Box::new(interpreter));
        for statement in statements {
            self.get_module_interpreter(name)
                .evaluate(statement)?;
        }
        self.state.borrow_mut().insert_top(name, Value::Module(name));
        Ok(())
    }

    fn variable_assignment(
        &'a self,
        name: &'a str,
        id: usize,
        expression: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        match self.locals.get(&id) {
            Some(env) => {
                let value = self.evaluate_expression(expression)?;
                self.state.borrow_mut().assign_at(*env, name, &value);
                Ok(value)
            }
            None => Err(ProgramError {
                location: location.clone(),
                message: format!("Variable `{}` not found!", name),
            }),
        }
    }

    fn call_expression(
        &'a self,
        callee: &'a Expression<'a>,
        arguments: &'a [Box<Expression<'a>>],
    ) -> EvaluationResult<'a> {
        let function_value = self.evaluate_expression(callee)?;
        match function_value {
            Value::Class(c) => {
                let instance = LoxObject::new(c);
                let mut values = vec![];
                for e in arguments {
                    let value = self.evaluate_expression(e)?;
                    values.push(value);
                }
                instance.init(&values, &self, &callee.location)?;
                Ok(Value::Object(instance))
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
            Value::Method(f, _) if f.arguments.len() != arguments.len() + 1 => Err(callee
                .create_program_error(
                    format!(
                        "Wrong number of arguments in method! Expected: {} Got: {}",
                        f.arguments.len(),
                        arguments.len()
                    )
                        .as_str(),
                )),
            Value::Method(f, this) => {
                let mut values = vec![Value::Object(this)];
                for e in arguments {
                    let value = self.evaluate_expression(e)?;
                    values.push(value);
                }
                f.eval(&values, &self)
            }
            Value::Function(f) => {
                let mut values = vec![];
                for e in arguments {
                    let value = self.evaluate_expression(e)?;
                    values.push(value);
                }
                f.eval(&values, &self)
            }
            _ => Err(callee.create_program_error("Only functions or classes can be called!")),
        }
    }

    fn conditional_expression(
        &'a self,
        condition: &'a Expression<'a>,
        then_branch: &'a Expression<'a>,
        else_branch: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let condition = self.evaluate_expression(condition)?;
        self.evaluate_expression(
             if condition.is_truthy() {
                 then_branch
             } else {
                 else_branch
             }
        )
    }

    fn boolean_expression(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        op: fn(Value<'a>, Value<'a>) -> Value<'a>,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        let right_value = self.evaluate_expression(right)?;
        Ok(op(left_value, right_value))
    }

    fn value_comparison_operation(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
        op: fn(f32, f32) -> bool,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        let right_value = self.evaluate_expression(right)?;
        comparison_operation(left_value, right_value, op)
            .map_err(|e| e.into_program_error(location))
    }

    fn eq_expressions(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        let right_value = self.evaluate_expression(right)?;
        Ok(Value::Boolean {
            value: left_value == right_value,
        })
    }

    fn value_math_operation(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
        i64_op: fn(i64, i64) -> i64,
        f32_op: fn(f32, f32) -> f32,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        let right_value = self.evaluate_expression(right)?;
        math_operation(left_value, right_value, i64_op, f32_op)
            .map_err(|e| e.into_program_error(location))
    }

    fn div_expressions(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        let right_value = self.evaluate_expression(right)?;
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
            .map_err(|e| e.into_program_error(&location))
    }

    fn add_expressions(
        &'a self,
        left: &'a Expression<'a>,
        right: &'a Expression<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        let left_value = self.evaluate_expression(left)?;
        if left_value.is_number() {
            let right_value = self.evaluate_expression(right)?;
            math_operation(left_value, right_value, i64::add, f32::add)
                .map_err(|e| e.into_program_error(location))
        } else {
            let left_string: String = left_value
                .try_into()
                .map_err(|e: ValueError| e.into_program_error(location))?;
            let right_value = self.evaluate_expression(right)?;
            let right_string: String = right_value
                .try_into()
                .map_err(|e: ValueError| e.into_program_error(location))?;
            Ok(Value::String {
                value: format!("{}{}", left_string, right_string),
            })
        }
    }

    fn get_property(
        &'a self,
        callee: &'a Expression<'a>,
        property: &'a str,
    ) -> EvaluationResult<'a> {
        let object = self.evaluate_expression(callee)?;
        match object {
            Value::Object(instance) => {
                if let Some(v) = instance.get(property) {
                    Ok(v)
                } else {
                    if let Some(v) = instance.get_getter(property) {
                        v.eval(&[Value::Object(instance)], &self)
                    } else {
                        Err(callee
                            .create_program_error(format!("Undefined property {}.", property).as_str()))
                    }
                }
            }
            Value::Class(c) => {
                if let Some(v) = c.static_instance.get(property) {
                    Ok(v)
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
        callee: &'a Expression<'a>,
        property: &'a str,
        value: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        let object = self.evaluate_expression(callee)?;
        if let Value::Object(instance) = object {
            let value = self.evaluate_expression(value)?;
            if let Some(f) = instance.get_setter(property) {
                f.eval(&[Value::Object(instance), value], &self)
            } else {
                instance.set(property, value.clone());
                Ok(value)
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
    ) -> EvaluationResult<'a> {
        self.array_element_operation(
            array, index, |array, index_value| {
                let value = self.evaluate_expression(value)?;
                array.borrow_mut().elements[index_value] = Box::new(value.clone());
                Ok(value)
            }
        )
    }

    fn array_element_expression(
        &'a self,
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
    ) -> EvaluationResult<'a> {
        self.array_element_operation(
            array, index, |array, index_value| {
                Ok(*array.borrow().elements[index_value].clone())
            }
        )
    }

    fn array_element_operation<I: Fn(Rc<RefCell<LoxArray<'a>>>, usize) -> EvaluationResult<'a>>(
        &'a self,
        array: &'a Expression<'a>,
        index: &'a Expression<'a>,
        op: I,
    ) -> EvaluationResult<'a> {
        let array_value = self.evaluate_expression(array)?;
        if let Value::Array(a) = array_value {
            let index_value = self.evaluate_expression(index)?;
            let index_value: i64 = i64::try_from(index_value).map_err(|e: ValueError| index.create_program_error(
                e.to_string().as_str(),
            ))?;
            if (index_value as usize) < a.borrow().capacity {
                op(a, index_value as usize)
            } else {
                Err(index.create_program_error(
                    format!(
                        "You can't access element {} in an array of {} elements",
                        index_value, a.borrow().capacity
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
    ) -> Option<Value<'a>> {
        if let Some(env) = self.locals.get(&expression_id) {
            self.state.borrow().get_at(name, *env)
        } else {
            self.state.borrow().get_global(name)
        }
    }

    fn is_value_type(
        &'a self,
        value: &Value<'a>,
        checked_type: &'a Type<'a>,
        location: &SourceCodeLocation<'a>,
    ) -> EvaluationResult<'a> {
        match (value, checked_type) {
            (Value::Nil, Type::Nil) => Ok(Value::Boolean { value: true }),
            (Value::Nil, _) => Ok(Value::Boolean { value: false }),
            (Value::Boolean { .. }, Type::Boolean) => Ok(Value::Boolean { value: true }),
            (Value::Boolean { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Integer { .. }, Type::Integer) => Ok(Value::Boolean { value: true }),
            (Value::Integer { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Float { .. }, Type::Float) => Ok(Value::Boolean { value: true }),
            (Value::Float { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Module { .. }, Type::Module) => Ok(Value::Boolean { value: true }),
            (Value::Module { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::String { .. }, Type::String) => Ok(Value::Boolean { value: true }),
            (Value::String { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Array { .. }, Type::Array) => Ok(Value::Boolean { value: true }),
            (Value::Array { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Function(_), Type::Function) => Ok(Value::Boolean { value: true }),
            (Value::Function(_), _) => Ok(Value::Boolean { value: false }),
            (Value::Trait { .. }, Type::Trait) => Ok(Value::Boolean { value: true }),
            (Value::Trait { .. }, _) => Ok(Value::Boolean { value: false }),
            (Value::Class(_), Type::Class) => Ok(Value::Boolean { value: true }),
            (Value::Class(_), _) => Ok(Value::Boolean { value: false }),
            (Value::Object(obj), Type::UserDefined(c)) => {
                let v = self.evaluate_expression(c)?;
                match v {
                    Value::Class(lox_class) => {
                        Ok(Value::Boolean { value: is_class(&lox_class, obj) })
                    }
                    Value::Trait(t) => {
                        Ok(Value::Boolean { value: is_trait(&t.name, obj) })
                    }
                    _ => Err(ProgramError {
                        location: c.location.clone(),
                        message: "Objects can only be either an implementation of a class or an object".to_owned(),
                    })
                }
            },
            (Value::Object(_), _) => Ok(Value::Boolean { value: false }),
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
    use crate::state::State;
    use crate::value::Value;
    use ahash::{AHashMap as HashMap};
    use parser::types::{
        Expression, ExpressionType, Literal, ProgramError, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use super::common_test::{create_expression, get_variable};
    use crate::interpreter::Interpreter;
    use std::cell::RefCell;

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
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 1.0 }));
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
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 0.0 }));
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
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 0.0 }));
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
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 0.0 }));
        assert_eq!(interpreter.state.borrow().find("identifier1"), Some(Value::Float { value: 1.0 }));
        assert_eq!(interpreter.state.borrow().find("identifier2"), Some(Value::Float { value: 2.0 }));
    }

    #[test]
    fn test_variable_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::VariableDeclaration {
                expression: Some(create_expression_number(1.0, &location)),
                name: "identifier",
            },
            location,
        };
        let state = State::default();
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 1.0 }));
    }

    #[test]
    fn test_function_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::FunctionDeclaration {
                name: "function",
                arguments: vec![],
                body: vec![Box::new(Statement {
                    statement_type: StatementType::EOF,
                    location: location.clone(),
                })],
                context_variables: vec![],
            },
            location: location.clone(),
        };
        let state = State::default();
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        let value = interpreter.state.borrow().find("function").unwrap();
        match value {
            Value::Function(lf ) => {
                assert_eq!(lf.arguments, Vec::<String>::new());
                assert_eq!(
                    lf.body,
                    vec![&Statement {
                        statement_type: StatementType::EOF,
                        location: location.clone(),
                    }]
                );
                assert_eq!(lf.location, location);
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
        let mut interpreter = Interpreter::new(&[], "");
        let mut state = State::default();
        state.in_function = true;
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().return_value, Some(Box::new(Value::Float { value: 1.0 })));
    }

    #[test]
    fn test_return_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let mut interpreter = Interpreter::new(&[], "");
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location: location.clone(),
        };
        let state = State::default();
        interpreter.state = RefCell::new(state);
        let r = interpreter.evaluate(&statement);
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
        let mut interpreter = Interpreter::new(&[], "");
        let mut state = State::default();
        state.loop_count = 1;
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert!(interpreter.state.borrow().broke_loop);
        assert_eq!(interpreter.state.borrow().loop_count, 1);
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
        let r = interpreter.evaluate(&statement);
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
        interpreter.state = RefCell::new(state);
        interpreter.evaluate(&statement).unwrap();
        assert_eq!(interpreter.state.borrow().find("identifier"), Some(Value::Float { value: 10.0 }));
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
    use ahash::{AHashMap as HashMap};
    use crate::function::LoxFunction;
    use crate::interpreter::Interpreter;
    use crate::state::State;
    use crate::value::Value;
    use parser::types::{
        DataKeyword, Expression, ExpressionType, Literal, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use super::common_test::{create_expression, get_variable};
    use std::rc::Rc;
    use std::cell::RefCell;

    #[test]
    fn test_expression_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let interpreter = Interpreter::new(&[], "");
        let e = get_number(1.0, &location);
        let got = interpreter.evaluate_expression(&e).unwrap();
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
        let mut interpreter = Interpreter::new(&[], "");
        state.insert("variable", Value::Float { value: 1.0 });
        interpreter.state = RefCell::new(state);
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let mut interpreter = Interpreter::new(&[], "");
        interpreter.state = RefCell::new(state);
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let interpreter = Interpreter::new(&[], "");
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        interpreter.state = RefCell::new(state);
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
        let mut interpreter = Interpreter::new(&[], "");
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
            Value::Function(Rc::new(LoxFunction {
                arguments: vec![],
                environments: state.get_environments(),
                body: vec![&s],
                location,
            })),
        );
        interpreter.state = RefCell::new(state);
        let got = interpreter.evaluate_expression(&expression).unwrap();
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
