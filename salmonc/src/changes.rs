use ahash::{AHashMap as HashMap};
use parser::types::{ExpressionType, MutPass, ProgramError, Statement, StatementType, Expression};

pub struct Changes<'a> {
    changes: HashMap<usize, ExpressionType<'a>>,
    statement_changes: HashMap<usize, StatementType<'a>>,
}

impl<'a> Changes<'a> {
    pub(crate) fn new(changes: HashMap<usize, ExpressionType<'a>>, statement_changes: HashMap<usize, StatementType<'a>>) -> Changes<'a> {
        Changes {
            changes,
            statement_changes,
        }
    }
}

// FIXME: Convert this into an immutable datastructure
impl<'a> MutPass<'a, ()> for Changes<'a> {
    fn run(&mut self, ss: &'a mut [Statement<'a>]) -> Result<(), Vec<ProgramError<'a>>> {
        for s in ss.iter_mut() {
            self.pass(s)?;
        }
        Ok(())
    }

    fn pass(&mut self, statement: &'a mut Statement<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(new_statement_type) = self.statement_changes.remove(&statement.id()) {
            statement.statement_type = new_statement_type;
        }
        match &mut statement.statement_type {
            StatementType::Module { name, statements } =>
                self.pass_module(name, statements)?,
            StatementType::Import { name } => self.pass_import(name)?,
            StatementType::Block { body } => self.pass_block(body)?,
            StatementType::VariableDeclaration { expression, name } =>
                self.pass_variable_declaration(name, expression)?,
            StatementType::ClassDeclaration {
                name,
                methods,
                static_methods,
                setters,
                getters,
                superclass,
            } => self.pass_class_declaration(name, methods, static_methods, setters, getters, superclass)?,
            StatementType::TraitDeclaration { name, .. } =>
                self.pass_trait_declaration(name)?,
            StatementType::TraitImplementation {
                class_name,
                trait_name,
                methods,
                static_methods,
                setters,
                getters,
                ..
            } => self.pass_trait_implementation(class_name, trait_name, methods, static_methods, setters, getters)?,
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
                ..
            } => self.pass_function_declaration(name, arguments, body)?,
            StatementType::Expression { expression } => self.pass_expression_statement(expression)?,
            StatementType::If {
                condition,
                then,
                otherwise,
            } => self.pass_if(condition, then, otherwise)?,
            StatementType::PrintStatement { expression } => self.pass_print(expression)?,
            StatementType::Return { value } => self.pass_return(value)?,
            StatementType::While { condition, action } =>
                self.pass_while(condition, action)?,
            StatementType::Break => {}
            StatementType::EOF => {}
        };
        Ok(())
    }

    fn pass_expression(&mut self, expression: &'a mut Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        let expression_id = expression.id();
        if let Some(new_expression_type) = self.changes.remove(&expression_id) {
            expression.expression_type = new_expression_type;
        }
        match &mut expression.expression_type {
            ExpressionType::UpliftClassVariables(name) => self.pass_uplift_class_variables(name)?,
            ExpressionType::UpliftFunctionVariables(name) => self.pass_uplift_function_variables(name)?,
            ExpressionType::IsType { value, checked_type } =>
                self.pass_checked_type(value, checked_type)?,
            ExpressionType::ModuleLiteral {
                module, field,
            } => self.pass_module_literal(module, field)?,
            ExpressionType::Get { callee, .. } => self.pass_get(callee)?,
            ExpressionType::Set { callee, value, .. } =>
                self.pass_set(callee, value)?,
            ExpressionType::VariableLiteral { identifier } => self.pass_variable_literal(identifier, expression_id)?,
            ExpressionType::VariableAssignment {
                identifier,
                expression: expression_value,
            } => self.pass_variable_assignment(identifier, expression_value, expression_id)?,
            ExpressionType::Binary { left, right, operator } =>
                self.pass_binary(left, right, operator)?,
            ExpressionType::Call { callee, arguments } =>
                self.pass_call(callee, arguments, expression_id)?,
            ExpressionType::Grouping { expression } => self.pass_grouping(expression)?,
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => self.pass_conditional(condition, then_branch, else_branch)?,
            ExpressionType::Unary { operand, operator } => self.pass_unary(operand, operator)?,
            ExpressionType::ExpressionLiteral {
                value
            } => self.pass_expression_literal(value)?,
            ExpressionType::AnonymousFunction { arguments, body } =>
                self.pass_anonymous_function(arguments, body, expression_id)?,
            ExpressionType::RepeatedElementArray { element, length } =>
                self.pass_repeated_element_array(element, length)?,
            ExpressionType::Array { elements } => self.pass_array(elements)?,
            ExpressionType::ArrayElement { array, index } =>
                self.pass_array_element(array, index)?,
            ExpressionType::ArrayElementSet {
                array,
                index,
                value,
            } => self.pass_array_element_set(array, index, value)?,
        };
        Ok(())
    }
}
