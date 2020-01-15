use crate::class::{LoxClass, LoxObject};
use crate::function::LoxFunction;
use crate::state::State;
use crate::value::{Value, ValueError, LoxModule};
use parser::resolver::WithScopedVariables;
use parser::types::{
    Expression, ExpressionType, FunctionHeader, ProgramError, SourceCodeLocation, Statement,
    StatementType, TokenType,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
use parser::importer::WithImports;

pub type EvaluationResult = Result<(State, Value), ProgramError>;

fn array_element_expression_set(
    array: &Expression,
    index: &Expression,
    value: &Expression,
    state: State,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (ns, array_value) = array.evaluate(state, locals, imports)?;
    if let Value::Array { elements, capacity } = array_value {
        let (ns, index_value) = index.evaluate(ns, locals, imports)?;
        if let Value::Number { value: index_value } = index_value {
            if index_value - index_value as usize as f32 == 0f32 {
                if (index_value as usize) < capacity {
                    let (ns, value) = value.evaluate(ns, locals, imports)?;
                    elements.borrow_mut()[index_value as usize] = Box::new(value.clone());
                    Ok((ns, value))
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
                Err(index.create_program_error("You can only index with integers"))
            }
        } else {
            Err(index.create_program_error("You can only index with numbers"))
        }
    } else {
        Err(array.create_program_error("You can only index arrays"))
    }
}

fn array_element_expression(
    array: &Expression,
    index: &Expression,
    state: State,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (ns, array_value) = array.evaluate(state, locals, imports)?;
    if let Value::Array { elements, capacity } = array_value {
        let (ns, index_value) = index.evaluate(ns, locals, imports)?;
        if let Value::Number { value: index_value } = index_value {
            if index_value - index_value as usize as f32 == 0f32 {
                if (index_value as usize) < capacity {
                    Ok((ns, *elements.borrow()[index_value as usize].clone()))
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
                Err(index.create_program_error("You can only index with integers"))
            }
        } else {
            Err(index.create_program_error("You can only index with numbers"))
        }
    } else {
        Err(array.create_program_error("You can only index arrays"))
    }
}

fn div_expressions(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state, locals, imports)?;
    let (final_state, right_value) = right.evaluate(s, locals, imports)?;
    match right_value {
        Value::Number { value } if value == 0f32 => {
            Err(right.create_program_error("Division by zero!"))
        }
        _ => Ok(()),
    }?;
    math_operation(left_value, right_value, f32::div)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(&location))
}

fn add_expressions(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state, locals, imports)?;
    if left_value.is_number() {
        let (final_state, right_value) = right.evaluate(s, locals, imports)?;
        math_operation(left_value, right_value, f32::add)
            .map(|v| (final_state, v))
            .map_err(|e| e.into_program_error(location))
    } else {
        let left_string: String = left_value
            .try_into()
            .map_err(|e: ValueError| e.into_program_error(location))?;
        let (final_state, right_value) = right.evaluate(s, locals, imports)?;
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

fn operation<R>(l: Value, r: Value, op: fn(f32, f32) -> R) -> Result<R, ValueError> {
    let l_number = l.try_into()?;
    let r_number = r.try_into()?;
    Ok(op(l_number, r_number))
}

fn math_operation(l: Value, r: Value, op: fn(f32, f32) -> f32) -> Result<Value, ValueError> {
    Ok(Value::Number {
        value: operation(l, r, op)?,
    })
}

fn comparison_operation(l: Value, r: Value, op: fn(f32, f32) -> bool) -> Result<Value, ValueError> {
    Ok(Value::Boolean {
        value: operation(l, r, op)?,
    })
}

fn value_math_operation(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    op: fn(f32, f32) -> f32,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state, locals, imports)?;
    let (final_state, right_value) = right.evaluate(s, locals, imports)?;
    math_operation(left_value, right_value, op)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(location))
}

fn value_comparison_operation(
    state: State,
    left: &Expression,
    right: &Expression,
    location: &SourceCodeLocation,
    op: fn(f32, f32) -> bool,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state, locals, imports)?;
    let (final_state, right_value) = right.evaluate(s, locals, imports)?;
    comparison_operation(left_value, right_value, op)
        .map(|v| (final_state, v))
        .map_err(|e| e.into_program_error(location))
}

fn eq_expressions(
    state: State,
    left: &Expression,
    right: &Expression,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (next_state, left_value) = left.evaluate(state, locals, imports)?;
    let (final_state, right_value) = right.evaluate(next_state, locals, imports)?;
    Ok((
        final_state,
        Value::Boolean {
            value: left_value == right_value,
        },
    ))
}

fn conditional_expression(
    state: State,
    condition: &Expression,
    then_branch: &Expression,
    else_branch: &Expression,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, condition) = condition.evaluate(state, locals, imports)?;
    if condition.is_truthy() {
        then_branch
    } else {
        else_branch
    }
    .evaluate(s, locals, imports)
}

fn boolean_expression(
    state: State,
    left: &Expression,
    right: &Expression,
    op: fn(Value, Value) -> Value,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (s, left_value) = left.evaluate(state, locals, imports)?;
    let (final_state, right_value) = right.evaluate(s, locals, imports)?;
    Ok((final_state, op(left_value, right_value)))
}

fn variable_assignment(
    state: State,
    name: &str,
    id: usize,
    expression: &Expression,
    location: &SourceCodeLocation,
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    match locals.get(&id) {
        Some(env) => {
            let (s, value) = expression.evaluate(state, locals, imports)?;
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
    state: State,
    callee: &Expression,
    arguments: &[Box<Expression>],
    locals: &HashMap<usize, usize>,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (next_state, function_value) = callee.evaluate(state, locals, imports)?;
    match function_value {
        Value::Class(c) => {
            let instance = LoxObject::new(c);
            let mut values = vec![];
            let mut current_state = next_state;
            for e in arguments {
                let (value_status, value) = e.evaluate(current_state, locals, imports)?;
                current_state = value_status;
                values.push(value);
            }
            instance.init(&values, locals, imports, &callee.location)?;
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
                let (value_status, value) = e.evaluate(current_state, locals, imports)?;
                current_state = value_status;
                values.push(value);
            }
            f.eval(&values, locals, imports).map(|v| (current_state, v))
        }
        _ => Err(callee.create_program_error("Only functions or classes can be called!")),
    }
}

fn look_up_variable(
    expression_id: usize,
    name: &str,
    locals: &HashMap<usize, usize>,
    state: &State,
) -> Option<Value> {
    if let Some(env) = locals.get(&expression_id) {
        state.get_at(name, *env)
    } else {
        state.get_global(name)
    }
}

fn anonymous_function(
    state: State,
    arguments: &[String],
    body: &[Statement],
    location: SourceCodeLocation,
) -> EvaluationResult {
    let environments = state.get_environments();
    Ok((
        state,
        Value::Function(LoxFunction {
            arguments: arguments.to_vec(),
            body: body.to_vec(),
            environments,
            location,
        }),
    ))
}

fn get_property(
    state: State,
    locals: &HashMap<usize, usize>,
    callee: &Expression,
    property: &String,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (next_state, object) = callee.evaluate(state, locals, imports)?;
    match object {
        Value::Object(instance) => {
            if let Some(v) = instance.get(property) {
                Ok((next_state, v))
            } else {
                if let Some(v) = instance.get_getter(property) {
                    v.eval(&[], locals, imports).map(|v| (next_state, v))
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
    state: State,
    locals: &HashMap<usize, usize>,
    callee: &Expression,
    property: &String,
    value: &Expression,
    imports: &HashMap<String, LoxModule>,
) -> EvaluationResult {
    let (ts, object) = callee.evaluate(state, locals, imports)?;
    if let Value::Object(mut instance) = object {
        let (final_state, value) = value.evaluate(ts, locals, imports)?;
        if let Some(f) = instance.get_setter(property) {
            f.eval(&[value], locals, imports).map(|v| (final_state, v))
        } else {
            instance.set(property.clone(), value.clone());
            Ok((final_state, value))
        }
    } else {
        Err(callee.create_program_error("Only instances have properties"))
    }
}

fn statements_to_hash_set(statements: &[&Statement]) -> HashSet<FunctionHeader> {
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

fn check_trait_methods(
    impl_methods: &[&Statement],
    trait_methods: &[FunctionHeader],
    location: &SourceCodeLocation,
) -> Result<(), Vec<ProgramError>> {
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

pub struct Interpreter {
    content: Vec<Statement>,
    locals: HashMap<usize, usize>,
    modules: HashMap<String, LoxModule>
}

impl Interpreter {
    pub fn new(content: Vec<Statement>) -> Interpreter {
        Interpreter {
            content,
            locals: HashMap::default(),
            modules: HashMap::default(),
        }
    }

    pub fn content(&self) -> &[Statement] {
        &self.content
    }

    pub fn run(&self) -> Result<State, ProgramError> {
        let mut current_state = State::default();
        for s in self.content.iter() {
            match s.evaluate(current_state, &self.locals, &self.modules) {
                Ok((next_state, _)) => {
                    current_state = next_state;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(current_state)
    }
}

impl WithScopedVariables for Interpreter {
    fn resolve_variable(&mut self, expression: &Expression, scope_id: usize) {
        self.locals.insert(expression.id(), scope_id);
    }
}

impl WithImports for Interpreter {
    fn add_import(&mut self, name: &str, statements: Vec<Statement>, locals: &HashMap<usize, usize>) -> Result<(), Vec<ProgramError>> {
        let mut interpreter = Interpreter::new(statements);
        interpreter.locals = locals.clone();
        let state = interpreter.run().map_err(|e| vec![e])?;
        self.modules.insert(name.to_owned(), LoxModule {
            locals: locals.clone(),
            state,
        });
        Ok(())
    }
}

pub trait Evaluable {
    fn evaluate(&self, state: State, locals: &HashMap<usize, usize>, imports: &HashMap<String, LoxModule>) -> EvaluationResult;
}

impl Evaluable for Expression {
    fn evaluate(&self, state: State, locals: &HashMap<usize, usize>, imports: &HashMap<String, LoxModule>) -> EvaluationResult {
        match &self.expression_type {
            ExpressionType::ModuleLiteral {
                module,
                field,
            } => {
                let value = look_up_variable(self.id(), module, locals, &state)
                    .ok_or_else(|| {
                        self.create_program_error(&format!("Module `{}` not found!", module))
                    })?
                    .clone();
                if let Value::Module(LoxModule {
                    state: module_state, locals: module_locals,
                }) = value {
                    let (s, v) = field.evaluate(module_state, &module_locals, imports)?;
                    state.assign_at(locals[&self.id()], module, &Value::Module(LoxModule {
                        state: s,
                        locals: module_locals,
                    }));
                    Ok((state, v))
                } else {
                    Err(self.create_program_error(&format!("Variable `{}` is not a module", module)))
                }
            }
            ExpressionType::ArrayElementSet {
                array,
                index,
                value,
            } => array_element_expression_set(array, index, value, state, locals, imports),
            ExpressionType::ArrayElement { array, index } => {
                array_element_expression(array, index, state, locals, imports)
            }
            ExpressionType::RepeatedElementArray { element, length } => {
                let (ns, element) = element.evaluate(state, locals, imports)?;
                let (ns, length) = length.evaluate(ns, locals, imports)?;
                if let Value::Number { value: length } = length {
                    let elements = Rc::new(RefCell::new(vec![Box::new(element); length as _]));
                    Ok((
                        ns,
                        Value::Array {
                            elements,
                            capacity: length as _,
                        },
                    ))
                } else {
                    Err(self.create_program_error("Array length should be a number"))
                }
            }
            ExpressionType::Array { elements } => {
                let (next_state, elements) =
                    elements
                        .iter()
                        .try_fold((state, vec![]), |(s, mut elements), e| {
                            let (ns, e) = e.evaluate(s, locals, imports)?;
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
            } => set_property(state, locals, callee, property, value, imports),
            ExpressionType::Get { callee, property } => {
                get_property(state, locals, callee, property, imports)
            }
            ExpressionType::ExpressionLiteral { value } => Ok((state, value.into())),
            ExpressionType::VariableLiteral { identifier } => {
                let value = look_up_variable(self.id(), identifier, locals, &state)
                    .ok_or_else(|| {
                        self.create_program_error(&format!("Variable `{}` not found!", identifier))
                    })?
                    .clone();
                if value == Value::Uninitialized {
                    Err(self.create_program_error(&format!(
                        "Variable `{}` not initialized!",
                        identifier
                    )))
                } else {
                    Ok((state, value))
                }
            }
            ExpressionType::Grouping { expression } => expression.evaluate(state, locals, imports),
            ExpressionType::Unary {
                operand,
                operator: TokenType::Minus,
            } => {
                let (s, v) = operand.evaluate(state, locals, imports)?;
                if v.is_number() {
                    Ok((s, -v))
                } else {
                    Err(self.create_program_error("Can only negate numbers"))
                }
            }
            ExpressionType::Unary {
                operand,
                operator: TokenType::Bang,
            } => operand.evaluate(state, locals, imports).map(|(s, v)| (s, !v)),
            ExpressionType::Unary { .. } => {
                Err(self.create_program_error("Invalid unary operator"))
            }
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Plus,
            } => add_expressions(state, left, right, &self.location, locals, imports),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Minus,
            } => value_math_operation(state, left, right, &self.location, f32::sub, locals, imports),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Slash,
            } => div_expressions(state, left, right, &self.location, locals, imports),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Star,
            } => value_math_operation(state, left, right, &self.location, f32::mul, locals, imports),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Greater,
            } => value_comparison_operation(
                state,
                left,
                right,
                &self.location,
                |f1, f2| f32::gt(&f1, &f2),
                locals,
                imports,
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::GreaterEqual,
            } => value_comparison_operation(
                state,
                left,
                right,
                &self.location,
                |f1, f2| f32::ge(&f1, &f2),
                locals,
                imports,
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Less,
            } => value_comparison_operation(
                state,
                left,
                right,
                &self.location,
                |f1, f2| f32::lt(&f1, &f2),
                locals,
                imports,
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::LessEqual,
            } => value_comparison_operation(
                state,
                left,
                right,
                &self.location,
                |f1, f2| f32::le(&f1, &f2),
                locals,
                imports,
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::EqualEqual,
            } => eq_expressions(state, left, right, locals, imports),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::BangEqual,
            } => eq_expressions(state, left, right, locals, imports).map(|(s, v)| (s, !v)),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::And,
            } => boolean_expression(
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
                locals,
                imports
            ),
            ExpressionType::Binary {
                left,
                right,
                operator: TokenType::Or,
            } => boolean_expression(
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
                locals,
                imports,
            ),
            ExpressionType::Binary { .. } => {
                Err(self.create_program_error("Invalid binary operator"))
            }
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => conditional_expression(state, condition, then_branch, else_branch, locals, imports),
            ExpressionType::VariableAssignment {
                expression,
                identifier,
            } => variable_assignment(
                state,
                identifier,
                self.id(),
                expression,
                &self.location,
                locals,
                imports,
            ),
            ExpressionType::Call { callee, arguments } => {
                call_expression(state, callee, arguments, locals, imports)
            }
            ExpressionType::AnonymousFunction { arguments, body } => {
                anonymous_function(state, arguments, body, self.location.clone())
            }
        }
    }
}

impl Evaluable for Statement {
    fn evaluate(
        &self,
        mut state: State,
        locals: &HashMap<usize, usize>,
        imports: &HashMap<String, LoxModule>,
    ) -> Result<(State, Value), ProgramError> {
        let state = match &self.statement_type {
            StatementType::EOF => state,
            StatementType::Import { name, } => if let Some(module) = imports.get(name) {
                state.insert_top(name.to_owned(), Value::Module(module.clone()));
                state
            } else {
                return Err(ProgramError {
                    message: format!("Failed to load module `{}`.", name),
                    location: self.location.clone(),
                });
            },
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                let (s, cond_value) = condition.evaluate(state, locals, imports)?;
                if cond_value.is_truthy() {
                    then.evaluate(s, locals, imports)?.0
                } else if let Some(o) = otherwise {
                    o.evaluate(s, locals, imports)?.0
                } else {
                    s
                }
            }
            StatementType::Expression { expression } => expression.evaluate(state, locals, imports)?.0,
            StatementType::Block { body } => {
                state.push();
                for st in body {
                    let (s, _) = st.evaluate(state, locals, imports)?;
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
                    e.evaluate(state, locals, imports)?
                } else {
                    (state, Value::Uninitialized)
                };
                s.insert_top(name.clone(), v);
                s
            }
            StatementType::PrintStatement { expression } => {
                let (s, v) = expression.evaluate(state, locals, imports)?;
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
                        name: name.clone(),
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
                if let (state, Value::Class(class)) = class_name.evaluate(state, locals, imports)? {
                    if let (
                        ns,
                        Value::Trait {
                            methods: trait_methods,
                            static_methods: trait_static_methods,
                            getters: trait_getters,
                            setters: trait_setters,
                            ..
                        },
                    ) = trait_name.evaluate(state, locals, imports)?
                    {
                        let methods = &methods.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let static_methods = &static_methods
                            .iter()
                            .map(|s| s.as_ref())
                            .collect::<Vec<&_>>();
                        let getters = &getters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let setters = &setters.iter().map(|s| s.as_ref()).collect::<Vec<&_>>();
                        let envs = ns.get_environments();
                        check_trait_methods(methods, &trait_methods, &self.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(methods, envs.clone());
                        check_trait_methods(static_methods, &trait_static_methods, &self.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_static_methods(static_methods, envs.clone());
                        check_trait_methods(getters, &trait_getters, &self.location)
                            .map_err(|ee| ee[0].clone())?;
                        class.append_methods(getters, envs.clone());
                        check_trait_methods(setters, &trait_setters, &self.location)
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
                    let (s, superclass) = e.evaluate(state, locals, imports)?;
                    if let Value::Class(c) = superclass {
                        (s, Some(c))
                    } else {
                        return Err(ProgramError {
                            message: "Superclass must be a class".to_owned(),
                            location: self.location.clone(),
                        });
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
                        body: body.iter().map(|s| (**s).clone()).collect(),
                        location: self.location.clone(),
                    }),
                );
                state
            }
            StatementType::Return { value } if state.in_function => match value {
                None => state,
                Some(e) => {
                    let (mut s, v) = e.evaluate(state, locals, imports)?;
                    s.add_return_value(v);
                    s
                }
            },
            StatementType::Return { .. } => {
                return Err(ProgramError {
                    location: self.location.clone(),
                    message: "Return outside function".to_owned(),
                })
            }
            StatementType::While { condition, action } => {
                state.loop_count += 1;
                while {
                    let (s, v) = condition.evaluate(state, locals, imports)?;
                    state = s;
                    state.loop_count > 0 && v.is_truthy()
                } {
                    let (s, _) = action.evaluate(state, locals, imports)?;
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
            StatementType::Break => {
                return Err(ProgramError {
                    location: self.location.clone(),
                    message: "Break outside loop".to_owned(),
                })
            }
        };
        Ok((state, Value::Nil))
    }
}

#[cfg(test)]
mod test_statement {
    use crate::function::LoxFunction;
    use crate::interpreter::Evaluable;
    use crate::state::State;
    use crate::value::Value;
    use parser::types::{
        Expression, ExpressionFactory, ExpressionType, Literal, ProgramError, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use std::collections::HashMap;

    fn create_expression(
        expression_type: ExpressionType,
        location: SourceCodeLocation,
    ) -> Expression {
        let mut factory = ExpressionFactory::new();
        factory.new_expression(expression_type, location)
    }

    #[test]
    fn test_if_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
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
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 1.0 }));
    }

    #[test]
    fn test_if_statement_else() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
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
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 0.0 }));
    }

    #[test]
    fn test_expression_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let statement = create_variable_assignment_statement("identifier", 0.0, &location);
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 0.0 }));
    }

    #[test]
    fn test_block_statement() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
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
        state.insert("identifier".to_owned(), Value::Number { value: 2.0 });
        state.insert("identifier1".to_owned(), Value::Number { value: 2.0 });
        state.insert("identifier2".to_owned(), Value::Number { value: 0.0 });
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 0.0 }));
        assert_eq!(s.find("identifier1"), Some(Value::Number { value: 1.0 }));
        assert_eq!(s.find("identifier2"), Some(Value::Number { value: 2.0 }));
    }

    #[test]
    fn test_variable_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let statement = Statement {
            statement_type: StatementType::VariableDeclaration {
                expression: Some(create_expression_number(1.0, &location)),
                name: "identifier".to_string(),
            },
            location,
        };
        let state = State::default();
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 1.0 }));
    }

    #[test]
    fn test_function_declaration() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let statement = Statement {
            statement_type: StatementType::FunctionDeclaration {
                name: "function".to_string(),
                arguments: vec![],
                body: vec![Box::new(Statement {
                    statement_type: StatementType::EOF,
                    location: location.clone(),
                })],
            },
            location: location.clone(),
        };
        let state = State::default();
        let (s, _) = statement.evaluate(state, &locals).unwrap();
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
                    vec![Statement {
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
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location,
        };
        let locals = HashMap::default();
        let mut state = State::default();
        state.in_function = true;
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.return_value, Some(Box::new(Value::Number { value: 1.0 })));
    }

    #[test]
    fn test_return_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let statement = Statement {
            statement_type: StatementType::Return {
                value: Some(create_expression_number(1.0, &location)),
            },
            location: location.clone(),
        };
        let state = State::default();
        let r = statement.evaluate(state, &locals);
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
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location,
        };
        let locals = HashMap::default();
        let mut state = State::default();
        state.loop_count = 1;
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert!(s.broke_loop);
        assert_eq!(s.loop_count, 1);
    }

    #[test]
    fn test_break_outside_function() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let statement = Statement {
            statement_type: StatementType::Break,
            location: location.clone(),
        };
        let locals = HashMap::default();
        let state = State::default();
        let r = statement.evaluate(state, &locals);
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
            file: "".to_owned(),
        };
        let identifier_expression = get_variable("identifier", &location);
        let mut locals = HashMap::default();
        locals.insert(0, 0);
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
                                identifier: "identifier".to_owned(),
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
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        let (s, _) = statement.evaluate(state, &locals).unwrap();
        assert_eq!(s.find("identifier"), Some(Value::Number { value: 10.0 }));
    }

    fn create_variable_assignment_statement(
        id: &str,
        value: f32,
        location: &SourceCodeLocation,
    ) -> Statement {
        Statement {
            statement_type: StatementType::Expression {
                expression: create_expression(
                    ExpressionType::VariableAssignment {
                        identifier: id.to_owned(),
                        expression: Box::new(create_expression_number(value, location)),
                    },
                    location.clone(),
                ),
            },
            location: location.clone(),
        }
    }

    fn create_expression_number(value: f32, location: &SourceCodeLocation) -> Expression {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::Number(value),
            },
            location.clone(),
        )
    }

    fn get_variable(s: &str, location: &SourceCodeLocation) -> Expression {
        create_expression(
            ExpressionType::VariableLiteral {
                identifier: s.to_owned(),
            },
            location.clone(),
        )
    }
}

#[cfg(test)]
mod test_expression {
    use crate::function::LoxFunction;
    use crate::interpreter::Evaluable;
    use crate::state::State;
    use crate::value::Value;
    use parser::types::{
        DataKeyword, Expression, ExpressionFactory, ExpressionType, Literal, SourceCodeLocation,
        Statement, StatementType, TokenType,
    };
    use std::collections::HashMap;

    #[test]
    fn test_expression_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let state = State::default();
        let (final_state, got) = get_number(1.0, &location)
            .evaluate(state.clone(), &locals)
            .unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_variable_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = get_variable("variable", &location);
        let mut state = State::default();
        state.insert("variable".to_owned(), Value::Number { value: 1.0 });
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_group_expression() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let expression = create_expression(
            ExpressionType::Grouping {
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let locals = HashMap::default();
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_minus_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Unary {
                operator: TokenType::Minus,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: -1.0 });
    }

    #[test]
    fn test_bang_operator() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Unary {
                operator: TokenType::Bang,
                operand: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: false });
    }

    #[test]
    fn test_sum() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_sum_strings() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Plus,
                left: Box::new(get_string("1", &location)),
                right: Box::new(get_string("2", &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
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
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Minus,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 0.0 });
    }

    #[test]
    fn test_mult() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Star,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_div() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Slash,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Greater,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_greater_equal_with_greater() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::GreaterEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Less,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(3.0, &location)),
                right: Box::new(get_number(3.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_less_equal_with_less() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::LessEqual,
                left: Box::new(get_number(1.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_equal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::EqualEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_different() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::BangEqual,
                left: Box::new(get_number(2.0, &location)),
                right: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_and() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::And,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(true, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_or() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Binary {
                operator: TokenType::Or,
                left: Box::new(get_boolean(true, &location)),
                right: Box::new(get_boolean(false, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Boolean { value: true });
    }

    #[test]
    fn test_conditional() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Conditional {
                condition: Box::new(get_boolean(true, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_conditional_else_branch() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Conditional {
                condition: Box::new(get_boolean(false, &location)),
                then_branch: Box::new(get_number(1.0, &location)),
                else_branch: Box::new(get_number(2.0, &location)),
            },
            location,
        );
        let state = State::default();
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 2.0 });
    }

    #[test]
    fn test_variable_assignment() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let mut locals = HashMap::default();
        locals.insert(0, 0);
        let expression = create_expression(
            ExpressionType::VariableAssignment {
                identifier: "identifier".to_owned(),
                expression: Box::new(get_number(1.0, &location)),
            },
            location,
        );
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        state.insert("identifier".to_owned(), Value::Number { value: 1.0 });
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Number { value: 1.0 });
    }

    #[test]
    fn test_function_call() {
        let location = SourceCodeLocation {
            line: 1,
            file: "".to_owned(),
        };
        let locals = HashMap::default();
        let expression = create_expression(
            ExpressionType::Call {
                callee: Box::new(get_variable("function", &location)),
                arguments: vec![],
            },
            location.clone(),
        );
        let mut state = State::default();
        state.insert("identifier".to_owned(), Value::Number { value: 0.0 });
        state.insert(
            "function".to_owned(),
            Value::Function(LoxFunction {
                arguments: vec![],
                environments: state.get_environments(),
                body: vec![Statement {
                    statement_type: StatementType::VariableDeclaration {
                        expression: Some(get_number(1.0, &location)),
                        name: "identifier".to_owned(),
                    },
                    location: location.clone(),
                }],
                location,
            }),
        );
        let (final_state, got) = expression.evaluate(state.clone(), &locals).unwrap();
        state.last().borrow_mut().remove("function");
        final_state.last().borrow_mut().remove("function");
        assert_eq!(state, final_state);
        assert_eq!(got, Value::Nil);
    }

    fn get_variable(s: &str, location: &SourceCodeLocation) -> Expression {
        create_expression(
            ExpressionType::VariableLiteral {
                identifier: s.to_owned(),
            },
            location.clone(),
        )
    }

    fn get_string(s: &str, location: &SourceCodeLocation) -> Expression {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::QuotedString(s.to_owned()),
            },
            location.clone(),
        )
    }

    fn get_number(n: f32, location: &SourceCodeLocation) -> Expression {
        create_expression(
            ExpressionType::ExpressionLiteral {
                value: Literal::Number(n),
            },
            location.clone(),
        )
    }

    fn get_boolean(n: bool, location: &SourceCodeLocation) -> Expression {
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

    fn create_expression(
        expression_type: ExpressionType,
        location: SourceCodeLocation,
    ) -> Expression {
        let mut factory = ExpressionFactory::new();
        factory.new_expression(expression_type, location)
    }
}
