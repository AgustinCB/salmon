use crate::types::{
    Expression, ExpressionType, ProgramError, SourceCodeLocation, Statement, StatementType,
};
use std::collections::HashMap;

pub trait WithScopedVariables {
    fn resolve_variable(&mut self, expression: &Expression, scope_id: usize);
}

pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    uses: Vec<HashMap<&'a str, usize>>,
    locations: Vec<HashMap<&'a str, &'a SourceCodeLocation>>,
    interpreter: &'a mut dyn WithScopedVariables,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut dyn WithScopedVariables) -> Resolver<'a> {
        Resolver {
            interpreter,
            locations: vec![HashMap::default()],
            scopes: vec![HashMap::default()],
            uses: vec![HashMap::default()],
        }
    }
    fn push_scope(&mut self, scope: HashMap<String, bool>) {
        self.scopes.push(scope);
        self.uses.push(HashMap::default());
        self.locations.push(HashMap::default());
    }
    fn pop_scope(&mut self) -> Result<(), Vec<ProgramError>> {
        self.scopes.pop();
        if let (Some(variables), Some(locations)) = (self.uses.pop(), self.locations.pop()) {
            let errors: Vec<ProgramError> = variables
                .iter()
                .filter(|(_, uses)| **uses == 0)
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
        location: &'a SourceCodeLocation,
    ) -> Result<(), ProgramError> {
        if let Some(s) = self.scopes.last_mut() {
            if s.contains_key(name) {
                return Err(ProgramError {
                    message: format!("Variable `{}` already declared in this scope!", name),
                    location: location.clone(),
                });
            }
            s.insert(name.to_owned(), false);
            if let Some(locations) = self.locations.last_mut() {
                locations.insert(name, location);
            }
            if let Some(uses) = self.uses.last_mut() {
                uses.insert(name, 0);
            }
        }
        Ok(())
    }
    fn define(&mut self, name: &str) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.to_owned(), true);
        }
    }
    fn resolve_local(&mut self, expression: &Expression, name: &'a str) {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find(|(_, s)| s.contains_key(name))
        {
            let new_uses = self.uses[i][name] + 1;
            self.uses[i].insert(name, new_uses);
            self.interpreter.resolve_variable(expression, i);
        }
    }
    fn resolve_functions(
        &mut self,
        methods: &'a [Box<Statement>],
        check_defined: bool,
    ) -> Result<(), Vec<ProgramError>> {
        methods
            .iter()
            .map(|s| {
                if let StatementType::FunctionDeclaration { name, .. } = &s.statement_type {
                    if !check_defined {
                        self.scopes.last_mut().map(|h| h.remove(name));
                    }
                }
                let r = self.resolve(s);
                if let StatementType::FunctionDeclaration { name, .. } = &s.statement_type {
                    self.uses.last_mut().unwrap().insert(name, 1);
                }
                r
            })
            .collect::<Result<_, Vec<ProgramError>>>()?;
        Ok(())
    }
    fn resolve_function<'b>(
        &mut self,
        arguments: &'a [String],
        body: &'b [&'a Statement],
        location: &'a SourceCodeLocation,
    ) -> Result<(), Vec<ProgramError>> {
        self.push_scope(HashMap::default());
        for arg in arguments {
            self.declare(arg, location).map_err(|e| vec![e])?;
            self.define(arg);
        }
        body.iter()
            .map(|s| self.resolve(s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        self.pop_scope()?;
        Ok(())
    }
    fn define_and_use(
        &mut self,
        variable: &'a str,
        location: &'a SourceCodeLocation,
    ) -> Result<(), Vec<ProgramError>> {
        self.declare(variable, location).map_err(|e| vec![e])?;
        self.define(variable);
        self.uses.last_mut().unwrap().insert(variable, 1);
        Ok(())
    }
}

pub trait Pass<'a> {
    fn run(&mut self, ss: &'a [Statement]) -> Result<(), Vec<ProgramError>>;
    fn resolve(&mut self, statement: &'a Statement) -> Result<(), Vec<ProgramError>>;
    fn resolve_expression(&mut self, expression: &'a Expression) -> Result<(), Vec<ProgramError>>;
}

impl<'a> Pass<'a> for Resolver<'a> {
    fn run(&mut self, ss: &'a [Statement]) -> Result<(), Vec<ProgramError>> {
        ss.iter()
            .map(|s| self.resolve(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        Ok(())
    }
    fn resolve(&mut self, statement: &'a Statement) -> Result<(), Vec<ProgramError>> {
        match &statement.statement_type {
            StatementType::Block { body } => {
                self.push_scope(HashMap::default());
                body.iter()
                    .map(|s| self.resolve(&s))
                    .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
                self.pop_scope()?;
            }
            StatementType::VariableDeclaration { expression, name } => {
                self.declare(name, &statement.location)
                    .map_err(|e| vec![e])?;
                if let Some(e) = expression {
                    self.resolve_expression(e)?;
                }
                self.define(&name);
            }
            StatementType::ClassDeclaration {
                name,
                methods,
                static_methods,
                setters,
                getters,
                superclass,
            } => {
                self.declare(name, &statement.location)
                    .map_err(|e| vec![e])?;
                if let Some(e) = superclass {
                    if let ExpressionType::VariableLiteral { identifier } = &e.expression_type {
                        if identifier == name {
                            return Err(vec![ProgramError {
                                message: "A class cannot inherit from itself.".to_owned(),
                                location: statement.location.clone(),
                            }]);
                        }
                        self.resolve_local(e, identifier);
                    }
                }
                self.push_scope(HashMap::default());
                self.define_and_use("this", &statement.location)?;
                if superclass.is_some() {
                    self.define_and_use("super", &statement.location)?;
                }
                self.resolve_functions(methods, true)?;
                self.resolve_functions(getters, false)?;
                self.resolve_functions(setters, false)?;
                self.pop_scope()?;
                self.resolve_functions(static_methods, true)?;
                self.define(&name);
            }
            StatementType::TraitDeclaration { name, .. } => {
                self.declare(name, &statement.location)
                    .map_err(|e| vec![e])?;
                self.define(name);
            }
            StatementType::TraitImplementation {
                class_name,
                trait_name,
                methods,
                static_methods,
                setters,
                getters,
                ..
            } => {
                self.resolve_expression(class_name)?;
                self.resolve_expression(trait_name)?;
                self.push_scope(HashMap::default());
                self.define_and_use("this", &statement.location)?;
                self.define_and_use("super", &statement.location)?;
                self.resolve_functions(methods, true)?;
                self.resolve_functions(getters, false)?;
                self.resolve_functions(setters, false)?;
                self.pop_scope()?;
                self.resolve_functions(static_methods, true)?;
            }
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            } => {
                self.declare(name, &statement.location)
                    .map_err(|e| vec![e])?;
                self.define(name);
                let body = body.iter().map(|s| &(**s)).collect::<Vec<&Statement>>();
                self.resolve_function(arguments, &body, &statement.location)?;
            }
            StatementType::Expression { expression } => {
                self.resolve_expression(expression)?;
            }
            StatementType::If {
                condition,
                then,
                otherwise,
            } => {
                self.resolve_expression(condition)?;
                self.resolve(then)?;
                if let Some(o) = otherwise {
                    self.resolve(o)?;
                }
            }
            StatementType::PrintStatement { expression } => {
                self.resolve_expression(expression)?;
            }
            StatementType::Return { value: Some(e) } => {
                self.resolve_expression(e)?;
            }
            StatementType::Return { .. } => {}
            StatementType::While { condition, action } => {
                self.resolve_expression(condition)?;
                self.resolve(action)?;
            }
            StatementType::Break => {}
            StatementType::EOF => {}
        };
        Ok(())
    }

    fn resolve_expression(&mut self, expression: &'a Expression) -> Result<(), Vec<ProgramError>> {
        match &expression.expression_type {
            ExpressionType::Get { callee, .. } => {
                self.resolve_expression(callee)?;
            }
            ExpressionType::Set { callee, value, .. } => {
                self.resolve_expression(callee)?;
                self.resolve_expression(value)?;
            }
            ExpressionType::VariableLiteral { identifier } => {
                self.resolve_local(expression, identifier);
            }
            ExpressionType::VariableAssignment {
                identifier,
                expression: expression_value,
            } => {
                self.resolve_expression(expression_value)?;
                self.resolve_local(expression, identifier);
            }
            ExpressionType::Binary { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            ExpressionType::Call { callee, arguments } => {
                self.resolve_expression(callee)?;
                arguments
                    .iter()
                    .map(|a| self.resolve_expression(a))
                    .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
            }
            ExpressionType::Grouping { expression } => {
                self.resolve_expression(expression)?;
            }
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_expression(then_branch)?;
                self.resolve_expression(else_branch)?;
            }
            ExpressionType::Unary { operand, .. } => {
                self.resolve_expression(operand)?;
            }
            ExpressionType::ExpressionLiteral { .. } => {}
            ExpressionType::AnonymousFunction { arguments, body } => {
                let body = body.iter().collect::<Vec<&'a Statement>>();
                self.resolve_function(arguments, &body, &expression.location)?;
            }
            ExpressionType::RepeatedElementArray { element, length } => {
                self.resolve_expression(length)?;
                self.resolve_expression(element)?;
            }
            ExpressionType::Array { elements } => {
                for element in elements {
                    self.resolve_expression(element)?;
                }
            }
            ExpressionType::ArrayElement { array, index } => {
                self.resolve_expression(array)?;
                self.resolve_expression(index)?;
            }
            ExpressionType::ArrayElementSet {
                array,
                index,
                value,
            } => {
                self.resolve_expression(array)?;
                self.resolve_expression(index)?;
                self.resolve_expression(value)?;
            }
        };
        Ok(())
    }
}
