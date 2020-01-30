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
pub enum Literal {
    QuotedString(String),
    Keyword(DataKeyword),
    Float(f32),
    Integer(i64),
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
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
    Identifier { name: String },
    TokenLiteral { value: Literal },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
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
        operator: TokenType,
        left: Box<Expression<'a>>,
    },
    Call {
        callee: Box<Expression<'a>>,
        arguments: Vec<Box<Expression<'a>>>,
    },
    Unary {
        operator: TokenType,
        operand: Box<Expression<'a>>,
    },
    Grouping {
        expression: Box<Expression<'a>>,
    },
    ExpressionLiteral {
        value: Literal,
    },
    ModuleLiteral {
        module: String,
        field: Box<Expression<'a>>,
    },
    VariableLiteral {
        identifier: String,
    },
    VariableAssignment {
        identifier: String,
        expression: Box<Expression<'a>>,
    },
    AnonymousFunction {
        arguments: Vec<String>,
        body: Vec<Statement<'a>>,
    },
    Get {
        callee: Box<Expression<'a>>,
        property: String,
    },
    Set {
        callee: Box<Expression<'a>>,
        property: String,
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
pub struct FunctionHeader {
    pub name: String,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType<'a> {
    Module {
        name: String,
        statements: Vec<Box<Statement<'a>>>,
    },
    Expression {
        expression: Expression<'a>,
    },
    PrintStatement {
        expression: Expression<'a>,
    },
    TraitDeclaration {
        name: String,
        methods: Vec<FunctionHeader>,
        getters: Vec<FunctionHeader>,
        setters: Vec<FunctionHeader>,
        static_methods: Vec<FunctionHeader>,
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
        name: String,
        superclass: Option<Expression<'a>>,
        methods: Vec<Box<Statement<'a>>>,
        static_methods: Vec<Box<Statement<'a>>>,
        getters: Vec<Box<Statement<'a>>>,
        setters: Vec<Box<Statement<'a>>>,
    },
    VariableDeclaration {
        expression: Option<Expression<'a>>,
        name: String,
    },
    FunctionDeclaration {
        name: String,
        arguments: Vec<String>,
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
        name: String,
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
}
