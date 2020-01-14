use std::fmt::{Debug, Display, Error, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceCodeLocation {
    pub file: String,
    pub line: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataKeyword {
    True,
    False,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    QuotedString(String),
    Keyword(DataKeyword),
    Number(f32),
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquareBrace,
    RightSquareBrace,
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
    Identifier { name: String },
    TokenLiteral { value: Literal },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub location: SourceCodeLocation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProgramError {
    pub location: SourceCodeLocation,
    pub message: String,
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(
            format!(
                "There was an error! [line {}] Error: {}",
                self.location.line + 1,
                self.message
            )
            .as_str(),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression {
    pub expression_type: ExpressionType,
    pub location: SourceCodeLocation,
    id: usize,
}

impl Expression {
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
    pub fn new_expression(
        &mut self,
        expression_type: ExpressionType,
        location: SourceCodeLocation,
    ) -> Expression {
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
pub enum ExpressionType {
    Conditional {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    Binary {
        right: Box<Expression>,
        operator: TokenType,
        left: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
    Unary {
        operator: TokenType,
        operand: Box<Expression>,
    },
    Grouping {
        expression: Box<Expression>,
    },
    ExpressionLiteral {
        value: Literal,
    },
    VariableLiteral {
        identifier: String,
    },
    VariableAssignment {
        identifier: String,
        expression: Box<Expression>,
    },
    AnonymousFunction {
        arguments: Vec<String>,
        body: Vec<Statement>,
    },
    Get {
        callee: Box<Expression>,
        property: String,
    },
    Set {
        callee: Box<Expression>,
        property: String,
        value: Box<Expression>,
    },
    Array {
        elements: Vec<Box<Expression>>,
    },
    RepeatedElementArray {
        element: Box<Expression>,
        length: Box<Expression>,
    },
    ArrayElement {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    ArrayElementSet {
        array: Box<Expression>,
        index: Box<Expression>,
        value: Box<Expression>,
    },
}

impl Expression {
    pub fn create_program_error(&self, message: &str) -> ProgramError {
        ProgramError {
            location: self.location.clone(),
            message: message.to_owned(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub location: SourceCodeLocation,
    pub statement_type: StatementType,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionHeader {
    pub name: String,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType {
    Expression {
        expression: Expression,
    },
    PrintStatement {
        expression: Expression,
    },
    TraitDeclaration {
        name: String,
        methods: Vec<FunctionHeader>,
        getters: Vec<FunctionHeader>,
        setters: Vec<FunctionHeader>,
        static_methods: Vec<FunctionHeader>,
    },
    TraitImplementation {
        trait_name: Expression,
        class_name: Expression,
        methods: Vec<Box<Statement>>,
        static_methods: Vec<Box<Statement>>,
        getters: Vec<Box<Statement>>,
        setters: Vec<Box<Statement>>,
    },
    ClassDeclaration {
        name: String,
        superclass: Option<Expression>,
        methods: Vec<Box<Statement>>,
        static_methods: Vec<Box<Statement>>,
        getters: Vec<Box<Statement>>,
        setters: Vec<Box<Statement>>,
    },
    VariableDeclaration {
        expression: Option<Expression>,
        name: String,
    },
    FunctionDeclaration {
        name: String,
        arguments: Vec<String>,
        body: Vec<Box<Statement>>,
    },
    Block {
        body: Vec<Box<Statement>>,
    },
    If {
        condition: Expression,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        action: Box<Statement>,
    },
    Return {
        value: Option<Expression>,
    },
    Break,
    EOF,
}
