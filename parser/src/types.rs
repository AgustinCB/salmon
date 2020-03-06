use std::fmt::{Debug, Display, Error, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceCodeLocation<'a> {
    pub file: &'a str,
    pub line: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataKeyword {
    True,
    False,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'a> {
    QuotedString(&'a str),
    Keyword(DataKeyword),
    Float(f32),
    Integer(i64),
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquareBrace,
    RightSquareBrace,
    DoubleColon,
    Colon,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Class,
    Else,
    Fun,
    For,
    Break,
    If,
    Or,
    Print,
    Question,
    Return,
    Var,
    While,
    Comment,
    EOF,
    Setter,
    Getter,
    Trait,
    Import,
    Arrow,
    Match,
    IsType,
    UppercaseNil,
    Boolean,
    Integer,
    Float,
    String,
    Function,
    UppercaseClass,
    UppercaseTrait,
    Array,
    Module,
    Mod,
    Identifier { name: &'a str },
    TokenLiteral { value: Literal<'a> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    pub location: SourceCodeLocation<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProgramError<'a> {
    pub location: SourceCodeLocation<'a>,
    pub message: String,
}

impl<'a> Display for ProgramError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(
            format!(
                "There was an error! [file {} line {}] Error: {}",
                self.location.file,
                self.location.line + 1,
                self.message
            )
            .as_str(),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression<'a> {
    pub expression_type: ExpressionType<'a>,
    pub location: SourceCodeLocation<'a>,
    id: usize,
}

impl<'a> Expression<'a> {
    pub fn id(&self) -> usize {
        self.id
    }
}

pub struct ExpressionFactory {
    counter: usize,
}

impl ExpressionFactory {
    pub fn new() -> ExpressionFactory {
        ExpressionFactory { counter: 0 }
    }
    #[cfg(test)]
    pub fn new_starting(counter: usize) -> ExpressionFactory {
        ExpressionFactory { counter }
    }
    pub fn new_expression<'a>(
        &mut self,
        expression_type: ExpressionType<'a>,
        location: SourceCodeLocation<'a>,
    ) -> Expression<'a> {
        let result = Expression {
            expression_type,
            location,
            id: self.counter,
        };
        self.counter += 1;
        result
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type<'a> {
    Nil,
    Boolean,
    Integer,
    Float,
    String,
    Function,
    Class,
    Trait,
    Array,
    Module,
    UserDefined(Box<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionType<'a> {
    Conditional {
        condition: Box<Expression<'a>>,
        then_branch: Box<Expression<'a>>,
        else_branch: Box<Expression<'a>>,
    },
    Binary {
        right: Box<Expression<'a>>,
        operator: TokenType<'a>,
        left: Box<Expression<'a>>,
    },
    Call {
        callee: Box<Expression<'a>>,
        arguments: Vec<Box<Expression<'a>>>,
    },
    Unary {
        operator: TokenType<'a>,
        operand: Box<Expression<'a>>,
    },
    Grouping {
        expression: Box<Expression<'a>>,
    },
    ExpressionLiteral {
        value: Literal<'a>,
    },
    ModuleLiteral {
        module: &'a str,
        field: Box<Expression<'a>>,
    },
    VariableLiteral {
        identifier: &'a str,
    },
    VariableAssignment {
        identifier: &'a str,
        expression: Box<Expression<'a>>,
    },
    AnonymousFunction {
        arguments: Vec<&'a str>,
        body: Vec<Statement<'a>>,
    },
    Get {
        callee: Box<Expression<'a>>,
        property: &'a str,
    },
    Set {
        callee: Box<Expression<'a>>,
        property: &'a str,
        value: Box<Expression<'a>>,
    },
    Array {
        elements: Vec<Box<Expression<'a>>>,
    },
    RepeatedElementArray {
        element: Box<Expression<'a>>,
        length: Box<Expression<'a>>,
    },
    ArrayElement {
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    ArrayElementSet {
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
    },
    IsType {
        value: Box<Expression<'a>>,
        checked_type: Type<'a>,
    },
}

impl<'a> Expression<'a> {
    pub fn create_program_error(&self, message: &str) -> ProgramError<'a> {
        ProgramError {
            location: self.location.clone(),
            message: message.to_owned(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement<'a> {
    pub location: SourceCodeLocation<'a>,
    pub statement_type: StatementType<'a>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionHeader<'a> {
    pub name: &'a str,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType<'a> {
    Module {
        name: &'a str,
        statements: Vec<Box<Statement<'a>>>,
    },
    Expression {
        expression: Expression<'a>,
    },
    PrintStatement {
        expression: Expression<'a>,
    },
    TraitDeclaration {
        name: &'a str,
        methods: Vec<FunctionHeader<'a>>,
        getters: Vec<FunctionHeader<'a>>,
        setters: Vec<FunctionHeader<'a>>,
        static_methods: Vec<FunctionHeader<'a>>,
    },
    TraitImplementation {
        trait_name: Expression<'a>,
        class_name: Expression<'a>,
        methods: Vec<Box<Statement<'a>>>,
        static_methods: Vec<Box<Statement<'a>>>,
        getters: Vec<Box<Statement<'a>>>,
        setters: Vec<Box<Statement<'a>>>,
    },
    ClassDeclaration {
        name: &'a str,
        superclass: Option<Expression<'a>>,
        methods: Vec<Box<Statement<'a>>>,
        static_methods: Vec<Box<Statement<'a>>>,
        getters: Vec<Box<Statement<'a>>>,
        setters: Vec<Box<Statement<'a>>>,
    },
    VariableDeclaration {
        expression: Option<Expression<'a>>,
        name: &'a str,
    },
    FunctionDeclaration {
        name: &'a str,
        arguments: Vec<&'a str>,
        body: Vec<Box<Statement<'a>>>,
    },
    Block {
        body: Vec<Box<Statement<'a>>>,
    },
    If {
        condition: Expression<'a>,
        then: Box<Statement<'a>>,
        otherwise: Option<Box<Statement<'a>>>,
    },
    While {
        condition: Expression<'a>,
        action: Box<Statement<'a>>,
    },
    Return {
        value: Option<Expression<'a>>,
    },
    Import {
        name: &'a str,
    },
    Break,
    EOF,
}

impl<'a> Statement<'a> {
    pub fn create_program_error(&self, message: &str) -> ProgramError<'a> {
        ProgramError {
            location: self.location.clone(),
            message: message.to_owned(),
        }
    }
}

pub trait Pass<'a, R> {
    fn run(&mut self, ss: &'a [Statement<'a>]) -> Result<R, Vec<ProgramError<'a>>>;

    fn pass(&mut self, statement: &'a Statement<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        match &statement.statement_type {
            StatementType::Module { name, statements } =>
                self.pass_module(name, statements, statement)?,
            StatementType::Import { name } => self.pass_import(name, statement)?,
            StatementType::Block { body } => self.pass_block(body)?,
            StatementType::VariableDeclaration { expression, name } =>
                self.pass_variable_declaration(name, expression, statement)?,
            StatementType::ClassDeclaration {
                name,
                methods,
                static_methods,
                setters,
                getters,
                superclass,
            } => self.pass_class_declaration(name, methods, static_methods, setters, getters, superclass, statement)?,
            StatementType::TraitDeclaration { name, .. } =>
                self.pass_trait_declaration(name, statement)?,
            StatementType::TraitImplementation {
                class_name,
                trait_name,
                methods,
                static_methods,
                setters,
                getters,
                ..
            } => self.pass_trait_implementation(class_name, trait_name, methods, static_methods, setters, getters, statement)?,
            StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            } => self.pass_function_declaration(name, arguments, body, statement)?,
            StatementType::Expression { expression } => self.pass_expression(expression)?,
            StatementType::If {
                condition,
                then,
                otherwise,
            } => self.pass_if(condition, then, otherwise)?,
            StatementType::PrintStatement { expression } => self.pass_print(expression)?,
            StatementType::Return { value: Some(e) } => self.pass_return(e)?,
            StatementType::Return { .. } => {}
            StatementType::While { condition, action } =>
                self.pass_while(condition, action)?,
            StatementType::Break => {}
            StatementType::EOF => {}
        };
        Ok(())
    }

    fn pass_expression(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        match &expression.expression_type {
            ExpressionType::IsType { value, checked_type } =>
                self.pass_checked_type(value, checked_type)?,
            ExpressionType::ModuleLiteral {
                module, field,
            } => self.pass_module_literal(module, field, expression)?,
            ExpressionType::Get { callee, .. } => self.pass_get(callee)?,
            ExpressionType::Set { callee, value, .. } =>
                self.pass_set(callee, value)?,
            ExpressionType::VariableLiteral { identifier } => self.pass_variable_literal(identifier, expression)?,
            ExpressionType::VariableAssignment {
                identifier,
                expression: expression_value,
            } => self.pass_variable_assignment(identifier, expression_value, expression)?,
            ExpressionType::Binary { left, right, .. } =>
                self.pass_binary(left, right)?,
            ExpressionType::Call { callee, arguments } =>
                self.pass_call(callee, arguments)?,
            ExpressionType::Grouping { expression } => self.pass_grouping(expression)?,
            ExpressionType::Conditional {
                condition,
                then_branch,
                else_branch,
            } => self.pass_conditional(condition, then_branch, else_branch)?,
            ExpressionType::Unary { operand, .. } => self.pass_unary(operand)?,
            ExpressionType::ExpressionLiteral { .. } => {}
            ExpressionType::AnonymousFunction { arguments, body } =>
                self.pass_anonymous_function(arguments, body, expression)?,
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

    fn pass_module(
        &mut self,
        _name: &'a str,
        statements: &'a [Box<Statement<'a>>],
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        statements.iter()
            .map(|s| self.pass(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        Ok(())
    }

    fn pass_import(
        &mut self,
        _name: &'a str,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        Ok(())
    }

    fn pass_block(
        &mut self,
        body: &'a [Box<Statement<'a>>],
    ) -> Result<(), Vec<ProgramError<'a>>> {
        body.iter()
            .map(|s| self.pass(&s))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        Ok(())
    }

    fn pass_variable_declaration(
        &mut self,
        _name: &'a str,
        expression: &'a Option<Expression<'a>>,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        if let Some(e) = expression {
            self.pass_expression(e)?;
        }
        Ok(())
    }

    fn pass_class_declaration(
        &mut self,
        _name: &'a str,
        methods: &'a [Box<Statement<'a>>],
        static_methods: &'a [Box<Statement<'a>>],
        setters: &'a [Box<Statement<'a>>],
        getters: &'a [Box<Statement<'a>>],
        superclass: &'a Option<Expression<'a>>,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        for ss in vec![methods, static_methods, setters, getters] {
            for s in ss {
                self.pass(s)?;
            }
        }
        if let Some(e) = superclass {
            self.pass_expression(e)?;
        }
        Ok(())
    }

    fn pass_trait_declaration(
        &mut self,
        _name: &'a str,
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
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
        for ss in vec![methods, static_methods, setters, getters] {
            for s in ss {
                self.pass(s)?;
            }
        }
        Ok(())
    }

    fn pass_function_declaration(
        &mut self,
        _name: &'a str,
        _arguments: &'a [&'a str],
        body: &'a [Box<Statement<'a>>],
        _statement: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        for s in body {
            self.pass(s)?;
        }
        Ok(())
    }

    fn pass_if(
        &mut self,
        condition: &'a Expression<'a>,
        then: &'a Statement<'a>,
        otherwise: &'a Option<Box<Statement<'a>>>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(condition)?;
        self.pass(then)?;
        if let Some(o) = otherwise {
            self.pass(o)?;
        }
        Ok(())
    }

    fn pass_print(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(expression)
    }

    fn pass_return(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(expression)
    }

    fn pass_while(
        &mut self,
        condition: &'a Expression<'a>,
        action: &'a Statement<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(condition)?;
        self.pass(action)
    }

    fn pass_checked_type(
        &mut self,
        value: &'a Expression<'a>,
        checked_type: &'a Type,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(value)?;
        if let Type::UserDefined(obj) = checked_type {
            self.pass_expression(obj)?;
        }
        Ok(())
    }

    fn pass_module_literal(
        &mut self,
        _module: &'a str,
        field: &'a Expression<'a>,
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(field)
    }

    fn pass_get(&mut self, callee: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(callee)
    }

    fn pass_set(
        &mut self,
        callee: &'a Expression<'a>,
        value: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(callee)?;
        self.pass_expression(value)
    }

    fn pass_variable_literal(
        &mut self,
        _identifier: &'a str,
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        Ok(())
    }

    fn pass_variable_assignment(
        &mut self,
        _identifier: &'a str,
        expression: &'a Expression<'a>,
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(expression)
    }

    fn pass_binary(&mut self, left: &'a Expression<'a>, right: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(left)?;
        self.pass_expression(right)
    }

    fn pass_call(
        &mut self,
        callee: &'a Expression<'a>,
        arguments: &'a [Box<Expression<'a>>],
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(callee)?;
        arguments
            .iter()
            .map(|a| self.pass_expression(a))
            .collect::<Result<Vec<()>, Vec<ProgramError>>>()?;
        Ok(())
    }

    fn pass_grouping(&mut self, expression: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(expression)
    }

    fn pass_conditional(
        &mut self,
        condition: &'a Expression<'a>,
        then_branch: &'a Expression<'a>,
        else_branch: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(condition)?;
        self.pass_expression(then_branch)?;
        self.pass_expression(else_branch)
    }

    fn pass_unary(&mut self, operand: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(operand)
    }

    fn pass_anonymous_function(
        &mut self,
        _arguments: &'a [&'a str],
        body: &'a [Statement<'a>],
        _expression: &'a Expression<'a>,
    ) -> Result<(), Vec<ProgramError<'a>>> {
        for s in body {
            self.pass(s)?;
        }
        Ok(())
    }

    fn pass_repeated_element_array(&mut self, element: &'a Expression<'a>, length: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(length)?;
        self.pass_expression(element)
    }

    fn pass_array(&mut self, elements: &'a [Box<Expression<'a>>]) -> Result<(), Vec<ProgramError<'a>>> {
        for element in elements {
            self.pass_expression(element)?;
        }
        Ok(())
    }

    fn pass_array_element(&mut self, array: &'a Expression<'a>, index: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(array)?;
        self.pass_expression(index)
    }

    fn pass_array_element_set(&mut self, array: &'a Expression<'a>, index: &'a Expression<'a>, value: &'a Expression<'a>) -> Result<(), Vec<ProgramError<'a>>> {
        self.pass_expression(array)?;
        self.pass_expression(index)?;
        self.pass_expression(value)
    }
}
