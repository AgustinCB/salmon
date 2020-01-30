use crate::types::{DataKeyword, Expression, ExpressionFactory, ExpressionType, FunctionHeader, Literal, ProgramError, SourceCodeLocation, Statement, StatementType, Token, TokenType, Type};
use std::cell::RefCell;
use std::iter::Peekable;

const INTERNAL_MATCH_VALUE_NAME: &str = "@match_value";

macro_rules! parse_branch {
    ($self: ident, $vec: ident, $location: ident, $value: expr, $next_location: ident) => {
        $self.consume(
            TokenType::Arrow,
            "Expecting `=>` after `*` on match!",
            &$location,
        )?;
        $vec.push(($value, $self.parse_block_statement($next_location)?));
        $self.consume(
            TokenType::Comma,
            "Expecting `,` at the end of branch on match!",
            &$location,
        )?;
    }
}

struct MethodSet<T> {
    getters: Vec<T>,
    methods: Vec<T>,
    setters: Vec<T>,
    static_methods: Vec<T>,
}

pub struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    block_stack: RefCell<u8>,
    content: RefCell<Peekable<I>>,
    expression_factory: RefCell<ExpressionFactory>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    pub fn new(content: Peekable<I>) -> Parser<'a, I> {
        Parser {
            block_stack: RefCell::new(0),
            expression_factory: RefCell::new(ExpressionFactory::new()),
            content: RefCell::new(content),
        }
    }

    pub fn parse(&self) -> Result<Vec<Statement<'a>>, Vec<ProgramError<'a>>> {
        let mut output_vec = vec![];
        let mut error_vec = vec![];

        while self.content.borrow_mut().peek().is_some() {
            match self.parse_statement() {
                Ok(s) => output_vec.push(s),
                Err(e) => error_vec.push(e),
            }
        }

        if error_vec.is_empty() {
            Ok(output_vec)
        } else {
            Err(error_vec)
        }
    }

    pub(crate) fn parse_statement(&self) -> Result<Statement<'a>, ProgramError<'a>> {
        match self.dry_next() {
            Some(Token {
                location,
                token_type: TokenType::Trait,
            }) => self.parse_trait_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::Import,
            }) => {
                self.next();
                let module = self.parse_identifier()?;
                self.consume(TokenType::Semicolon, "Expected `;` at the end of statement.", &location)?;
                Ok(Statement {
                    location,
                    statement_type: StatementType::Import {
                        name: module,
                    },
                })
            }
            Some(Token {
                location,
                token_type: TokenType::Class,
            }) => self.parse_class_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::If,
            }) => self.parse_if_statement(&location),
            Some(Token {
                 location,
                 token_type: TokenType::Match,
            }) => self.parse_match_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::Return,
            }) => self.parse_return_statement(location),
            Some(Token {
                location,
                token_type: TokenType::Var,
            }) => self.parse_var_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::LeftBrace,
            }) => self.parse_block_statement(location),
            Some(Token {
                location,
                token_type: TokenType::Fun,
            }) => self.parse_fun_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::While,
            }) => self.parse_while_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::For,
            }) => self.parse_for_statement(&location),
            Some(Token {
                location,
                token_type: TokenType::EOF,
            }) => Ok(Statement {
                location,
                statement_type: StatementType::EOF,
            }),
            Some(Token {
                location,
                token_type: TokenType::Mod
            }) => {
                self.next();
                let name = self.parse_identifier()?;
                let block = self.parse_block_statement(location.clone())?;
                if let StatementType::Block { body } = block.statement_type {
                    Ok(Statement {
                        location,
                        statement_type: StatementType::Module {
                            name,
                            statements: body,
                        }
                    })
                } else {
                    panic!("Cannot happen")
                }
            }
            Some(Token {
                location,
                token_type: TokenType::Print,
            }) => {
                self.next();
                let expression = self.parse_expression()?;
                self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
                Ok(Statement {
                    location,
                    statement_type: StatementType::PrintStatement { expression },
                })
            }
            Some(Token {
                location,
                token_type: TokenType::Break,
            }) => {
                self.next();
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon after break statement",
                    &location,
                )?;
                if *self.block_stack.borrow() > 0 {
                    Ok(Statement {
                        location,
                        statement_type: StatementType::Break,
                    })
                } else {
                    Err(ProgramError {
                        message: "Break statement can't go here".to_owned(),
                        location,
                    })
                }
            }
            None => Err(ProgramError {
                message: "Unexpected end of file".to_owned(),
                location: SourceCodeLocation {
                    line: 0,
                    file: "",
                },
            }),
            _ => {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::Semicolon,
                    "Expected semicolon",
                    &expression.location,
                )?;
                Ok(Statement {
                    location: expression.location.clone(),
                    statement_type: StatementType::Expression { expression },
                })
            }
        }
    }

    #[inline]
    fn dry_next(&self) -> Option<Token<'a>> {
        self.content.borrow_mut().peek().cloned()
    }

    #[inline]
    fn next(&self) -> Option<Token<'a>> {
        self.content.borrow_mut().next()
    }

    #[cfg(test)]
    #[inline]
    fn is_empty(&self) -> bool {
        self.content.borrow_mut().peek().is_none()
    }

    fn parse_method_set<T, C: Fn(&mut Vec<T>, &SourceCodeLocation<'a>) -> Result<(), ProgramError<'a>>>(
        &self,
        location: &SourceCodeLocation<'a>,
        action: C,
    ) -> Result<MethodSet<T>, ProgramError<'a>> {
        let mut methods = vec![];
        let mut static_methods = vec![];
        let mut setters = vec![];
        let mut getters = vec![];
        while !self.peek(TokenType::RightBrace) {
            let vector = match self.dry_next().map(|t| t.token_type) {
                Some(TokenType::Class) => {
                    self.next();
                    &mut static_methods
                }
                Some(TokenType::Getter) => {
                    self.next();
                    &mut getters
                }
                Some(TokenType::Setter) => {
                    self.next();
                    &mut setters
                }
                _ => &mut methods,
            };
            action(vector, location)?;
        }
        Ok(MethodSet {
            getters,
            methods,
            setters,
            static_methods,
        })
    }

    fn parse_trait_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.consume(TokenType::Trait, "Expected trait keyword", location)?;
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
        }) = self.next()
        {
            if self.peek(TokenType::LeftBrace) {
                self.parse_trait_declaration(name, &location)
            } else {
                self.parse_trait_implementation(name, &location)
            }
        } else {
            Err(ProgramError {
                message: "Expected trait name".to_owned(),
                location: location.clone(),
            })
        }
    }

    fn parse_trait_implementation(
        &self,
        trait_name: String,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        let trait_name = self.parse_variable_or_module_access(trait_name, &location)?;
        self.consume(TokenType::For, "Expected 'for' after trait name", location)?;
        let class_name = if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
        }) = self.next() {
            self.parse_variable_or_module_access(name, &location)
        } else {
            Err(ProgramError {
                location: location.clone(),
                message: "Expected object name for trait implementation".to_owned(),
            })
        }?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' before trait body",
            location,
        )?;
        let method_set = self.parse_class_methods(&location)?;
        self.consume(
            TokenType::RightBrace,
            "Expected '}' after trait body",
            location,
        )?;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::TraitImplementation {
                getters: method_set.getters,
                methods: method_set.methods,
                setters: method_set.setters,
                static_methods: method_set.static_methods,
                trait_name,
                class_name,
            },
        })
    }

    fn parse_trait_declaration(
        &self,
        name: String,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' before trait body",
            location,
        )?;
        let method_set = self.parse_method_set(location, |vector, location| {
            let (name, arguments) = self.parse_function_header(location)?;
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after function header",
                location,
            )?;
            vector.push(FunctionHeader {
                arity: arguments.len(),
                name,
            });
            Ok(())
        })?;
        self.consume(
            TokenType::RightBrace,
            "Expected '}' after trait body",
            location,
        )?;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::TraitDeclaration {
                getters: method_set.getters,
                methods: method_set.methods,
                name,
                setters: method_set.setters,
                static_methods: method_set.static_methods,
            },
        })
    }

    fn parse_class_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
        }) = self.next()
        {
            let superclass = if self.peek(TokenType::Less) {
                self.next();
                if let Some(TokenType::Identifier { name }) = self.next().map(|t| t.token_type) {
                    Some(self.parse_variable_or_module_access(name, &location)?)
                } else {
                    return Err(ProgramError {
                        message: "Expect superclass name.".to_owned(),
                        location: location.clone(),
                    });
                }
            } else {
                None
            };
            self.consume(
                TokenType::LeftBrace,
                "Expected '{' before class body",
                &location,
            )?;
            let method_set = self.parse_class_methods(&location)?;
            self.consume(
                TokenType::RightBrace,
                "Expected '}' after class body",
                &location,
            )?;

            Ok(Statement {
                statement_type: StatementType::ClassDeclaration {
                    getters: method_set.getters,
                    name,
                    methods: method_set.methods,
                    setters: method_set.setters,
                    static_methods: method_set.static_methods,
                    superclass,
                },
                location,
            })
        } else {
            Err(ProgramError {
                message: "Expected name in class definition".to_owned(),
                location: location.clone(),
            })
        }
    }

    fn parse_class_methods(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<MethodSet<Box<Statement<'a>>>, ProgramError<'a>> {
        let method_set = self.parse_method_set(&location, |vector, location| {
            let f = Box::new(self.parse_function(location)?);
            vector.push(f);
            Ok(())
        })?;
        for getter in method_set.getters.iter() {
            if let StatementType::FunctionDeclaration { arguments, .. } = &getter.statement_type {
                if !arguments.is_empty() {
                    return Err(ProgramError {
                        message: "Getter function should take no arguments".to_owned(),
                        location: getter.location.clone(),
                    });
                }
            }
        }
        for setter in method_set.setters.iter() {
            if let StatementType::FunctionDeclaration { arguments, .. } = &setter.statement_type {
                if arguments.len() != 1 {
                    return Err(ProgramError {
                        message: "Setter function should take one argument".to_owned(),
                        location: setter.location.clone(),
                    });
                }
            }
        }
        Ok(method_set)
    }

    fn parse_match_statement(&self, location: &SourceCodeLocation<'a>) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        let value = self.parse_expression()?;
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' before match branches",
            location,
        )?;
        let (literal_branches, types_branches) = self.parse_match_branches(location)?;
        if !literal_branches.is_empty() && !types_branches.is_empty() {
            return Err(ProgramError {
                message: "`match` statement mixes values and types in branches".to_owned(),
                location: location.clone(),
            })
        }
        self.consume(
            TokenType::Star,
            "No `*` branch on match!",
            &location,
        )?;
        self.consume(
            TokenType::Arrow,
            "Expecting `=>` after `*` on match!",
            &location,
        )?;
        let match_all = self.parse_statement()?;
        self.consume(
            TokenType::Comma,
            "Expecting `,` at the end of branch on match!",
            &location,
        )?;
        self.consume(
            TokenType::RightBrace,
            "Expected '}' after match branches",
            location,
        )?;
        let if_elses = if types_branches.is_empty() {
            self.build_if_literal_chain(literal_branches, match_all)
        } else {
            self.build_if_type_chain(types_branches, match_all)
        };
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::VariableDeclaration {
                            name: INTERNAL_MATCH_VALUE_NAME.to_owned(),
                            expression: Some(value),
                        }
                    }),
                    Box::new(if_elses),
                ]
            }
        })
    }

    fn build_if_type_chain(&self, branches: Vec<(Type<'a>, Statement<'a>)>, match_all: Statement<'a>) -> Statement<'a> {
        branches.into_iter()
            .fold(match_all, |acc, (checked_type, s)| {
                let value = Box::new(self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::VariableLiteral {
                        identifier: INTERNAL_MATCH_VALUE_NAME.to_owned()
                    },
                    s.location.clone(),
                ));
                Statement {
                    location: s.location.clone(),
                    statement_type: StatementType::If {
                        condition: self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::IsType {
                                value,
                                checked_type,
                            },
                            s.location.clone(),
                        ),
                        then: Box::new(s),
                        otherwise: Some(Box::new(acc)),
                    }
                }
            })
    }

    fn build_if_literal_chain(&self, branches: Vec<(Literal, Statement<'a>)>, match_all: Statement<'a>) -> Statement<'a> {
        branches.into_iter()
            .fold(match_all, |acc, (value, s)| {
                let left = Box::new(self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::ExpressionLiteral { value },
                    s.location.clone(),
                ));
                let right = Box::new(self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::VariableLiteral {
                        identifier: INTERNAL_MATCH_VALUE_NAME.to_owned()
                    },
                    s.location.clone(),
                ));
                Statement {
                    location: s.location.clone(),
                    statement_type: StatementType::If {
                        condition: self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::Binary {
                                operator: TokenType::EqualEqual,
                                left,
                                right,
                            },
                            s.location.clone(),
                        ),
                        then: Box::new(s),
                        otherwise: Some(Box::new(acc)),
                    }
                }
            })
    }

    fn parse_match_branches(
        &self, location: &SourceCodeLocation<'a>
    ) -> Result<(Vec<(Literal, Statement<'a>)>, Vec<(Type<'a>, Statement<'a>)>), ProgramError<'a>> {
        let mut branches = vec![];
        let mut types = vec![];
        while !self.peek(TokenType::Star) && !self.peek(TokenType::RightBrace) {
            let next = self.next();
            let next_location = next.clone().map_or(location.clone(), |n| n.location);
            match next.map(|t| t.token_type) {
                Some(TokenType::TokenLiteral { value }) => {
                    parse_branch!(self, branches, location, value, next_location);
                }
                Some(TokenType::UppercaseNil) => {
                    parse_branch!(self, types, location, Type::Nil, next_location);
                }
                Some(TokenType::Boolean) => {
                    parse_branch!(self, types, location, Type::Boolean, next_location);
                }
                Some(TokenType::Integer) => {
                    parse_branch!(self, types, location, Type::Integer, next_location);
                }
                Some(TokenType::Float) => {
                    parse_branch!(self, types, location, Type::Float, next_location);
                }
                Some(TokenType::String) => {
                    parse_branch!(self, types, location, Type::String, next_location);
                }
                Some(TokenType::Array) => {
                    parse_branch!(self, types, location, Type::Array, next_location);
                }
                Some(TokenType::Function) => {
                    parse_branch!(self, types, location, Type::Function, next_location);
                }
                Some(TokenType::Module) => {
                    parse_branch!(self, types, location, Type::Module, next_location);
                }
                Some(TokenType::UppercaseClass) => {
                    parse_branch!(self, types, location, Type::Class, next_location);
                }
                Some(TokenType::UppercaseTrait) => {
                    parse_branch!(self, types, location, Type::Trait, next_location);
                }
                Some(TokenType::Identifier { name }) => {
                    let obj = Box::new(self.parse_variable_or_module_access(name, location)?);
                    parse_branch!(self, types, location, Type::UserDefined(obj), next_location);
                }
                _ => return Err(ProgramError {
                    message: "All branches should match using literals".to_owned(),
                    location: next_location,
                }),
            }
        }
        Ok((branches, types))
    }

    fn parse_if_statement(&self, location: &SourceCodeLocation<'a>) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after if token",
            location,
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after if token",
            location,
        )?;
        let then = Box::new(self.parse_statement()?);
        let otherwise = if self.peek(TokenType::Else) {
            self.next();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::If {
                condition,
                then,
                otherwise,
            },
        })
    }

    fn parse_return_statement(
        &self,
        location: SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.content.borrow_mut().next();
        if self.peek(TokenType::Semicolon) {
            self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
            Ok(Statement {
                location,
                statement_type: StatementType::Return { value: None },
            })
        } else {
            let value = self.parse_expression()?;
            self.consume(TokenType::Semicolon, "Expected semicolon", &location)?;
            Ok(Statement {
                location,
                statement_type: StatementType::Return { value: Some(value) },
            })
        }
    }

    fn parse_var_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        if let Some(TokenType::Identifier { name }) = self.next().map(|t| t.token_type) {
            match self.next() {
                Some(Token {
                    token_type: TokenType::Semicolon,
                    ..
                }) => Ok(Statement {
                    location: location.clone(),
                    statement_type: StatementType::VariableDeclaration {
                        name,
                        expression: None,
                    },
                }),
                Some(Token {
                    token_type: TokenType::Equal,
                    ..
                }) => {
                    let expression = Some(self.parse_expression()?);
                    self.consume(TokenType::Semicolon, "Expected semicolon", location)?;
                    Ok(Statement {
                        location: location.clone(),
                        statement_type: StatementType::VariableDeclaration { name, expression },
                    })
                }
                _ => Err(ProgramError {
                    location: location.clone(),
                    message: "Invalid variable declaration!".to_owned(),
                }),
            }
        } else {
            Err(ProgramError {
                location: location.clone(),
                message: "Invalid variable declaration!".to_owned(),
            })
        }
    }

    fn parse_block_statement(
        &self,
        mut location: SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.consume(TokenType::LeftBrace, "Expected left brace", &location)?;
        let mut statements = vec![];
        *self.block_stack.borrow_mut() += 1;
        while !self.peek(TokenType::RightBrace) {
            let statement = self.parse_statement()?;
            location = statement.location.clone();
            statements.push(Box::new(statement));
        }
        self.consume(TokenType::RightBrace, "Expected '}' after block", &location)?;
        *self.block_stack.borrow_mut() -= 1;
        Ok(Statement {
            location,
            statement_type: StatementType::Block { body: statements },
        })
    }

    fn parse_anonymous_function(
        &self,
        location: SourceCodeLocation<'a>,
    ) -> Result<Expression<'a>, ProgramError<'a>> {
        self.next();
        let arguments = self.parse_parameters(&location, Parser::parse_identifier)?;
        self.consume(
            TokenType::RightParen,
            "Expected a parenthesis after parameters!",
            &location,
        )?;
        let body = if let StatementType::Block { body } =
            self.parse_block_statement(location.clone())?.statement_type
        {
            body.into_iter().map(|b| *b).collect::<Vec<Statement>>()
        } else {
            panic!("Can't happen")
        };
        Ok(self.expression_factory.borrow_mut().new_expression(
            ExpressionType::AnonymousFunction { arguments, body },
            location,
        ))
    }

    fn parse_fun_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.content.borrow_mut().next();
        self.parse_function(location)
    }

    fn parse_function_header(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<(String, Vec<String>), ProgramError<'a>> {
        if let Some(Token {
            token_type: TokenType::Identifier { name },
            location,
        }) = self.next()
        {
            self.consume(
                TokenType::LeftParen,
                "Expected a parenthesis after name!",
                &location,
            )?;
            let arguments = self.parse_parameters(&location, Parser::parse_identifier)?;
            self.consume(
                TokenType::RightParen,
                "Expected a parenthesis after parameters!",
                &location,
            )?;
            Ok((name, arguments))
        } else {
            Err(ProgramError {
                location: location.clone(),
                message: "Expected a function name!".to_owned(),
            })
        }
    }

    fn parse_function(&self, location: &SourceCodeLocation<'a>) -> Result<Statement<'a>, ProgramError<'a>> {
        let (name, arguments) = self.parse_function_header(location)?;
        let body = if let StatementType::Block { body } =
            self.parse_block_statement(location.clone())?.statement_type
        {
            body
        } else {
            panic!("Can't happen")
        };
        Ok(Statement {
            statement_type: StatementType::FunctionDeclaration {
                name,
                arguments,
                body,
            },
            location: location.clone(),
        })
    }

    fn parse_while_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after while token",
            location,
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after while condition",
            &condition.location,
        )?;
        *self.block_stack.borrow_mut() += 1;
        let body = self.parse_statement()?;
        *self.block_stack.borrow_mut() -= 1;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::While {
                condition,
                action: Box::new(body),
            },
        })
    }

    fn parse_for_statement(
        &self,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Statement<'a>, ProgramError<'a>> {
        self.next();
        self.consume(
            TokenType::LeftParen,
            "Expected '(' after while token",
            location,
        )?;
        let temp_init = match self.dry_next() {
            Some(Token {
                location,
                token_type: TokenType::Semicolon,
            }) => {
                self.next();
                Ok(Statement {
                    location: location.clone(),
                    statement_type: StatementType::Expression {
                        expression: self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::ExpressionLiteral {
                                value: Literal::Keyword(DataKeyword::Nil),
                            },
                            location,
                        ),
                    },
                })
            }
            Some(_) => self.parse_statement(),
            _ => Err(ProgramError {
                message: "Unexpected end of file".to_owned(),
                location: SourceCodeLocation {
                    line: 0,
                    file: "",
                },
            }),
        }?;
        let init = match &temp_init.statement_type {
            StatementType::VariableDeclaration { .. } | StatementType::Expression { .. } => {
                Ok(temp_init)
            }
            _ => Err(ProgramError {
                location: temp_init.location,
                message: "Invalid statement for initialization!".to_owned(),
            }),
        }?;
        let condition = {
            if !self.peek(TokenType::Semicolon) {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::Semicolon,
                    "Expecting ';' after expression.",
                    &expression.location,
                )?;
                expression
            } else {
                self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                    location.clone(),
                )
            }
        };
        let incr = {
            let expression = if !self.peek(TokenType::RightParen) {
                let expression = self.parse_expression()?;
                self.consume(
                    TokenType::RightParen,
                    "Expecting ')' after expression.",
                    &expression.location,
                )?;
                expression
            } else {
                self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::ExpressionLiteral {
                        value: Literal::Keyword(DataKeyword::Nil),
                    },
                    location.clone(),
                )
            };
            Statement {
                location: expression.location.clone(),
                statement_type: StatementType::Expression { expression },
            }
        };
        let body = self.parse_statement()?;
        Ok(Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(init),
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::While {
                            condition,
                            action: Box::new(Statement {
                                location: location.clone(),
                                statement_type: StatementType::Block {
                                    body: vec![Box::new(body), Box::new(incr)],
                                },
                            }),
                        },
                    }),
                ],
            },
        })
    }

    pub(crate) fn parse_expression(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_assignment()
    }

    fn parse_assignment(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        let variable = self.parse_ternary()?;
        match self.dry_next() {
            Some(Token {
                token_type: TokenType::Equal,
                location,
            }) => {
                self.next();
                if self.dry_next().is_some() {
                    let expression = self.parse_assignment()?;
                    match variable {
                        Expression {
                            expression_type: ExpressionType::VariableLiteral { identifier },
                            location,
                            ..
                        } => Ok(self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::VariableAssignment {
                                identifier,
                                expression: Box::new(expression),
                            },
                            location,
                        )),
                        Expression {
                            expression_type: ExpressionType::Get { callee, property },
                            location,
                            ..
                        } => Ok(self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::Set {
                                callee,
                                property,
                                value: Box::new(expression),
                            },
                            location,
                        )),
                        Expression {
                            expression_type: ExpressionType::ArrayElement { array, index },
                            location,
                            ..
                        } => Ok(self.expression_factory.borrow_mut().new_expression(
                            ExpressionType::ArrayElementSet {
                                array,
                                index,
                                value: Box::new(expression),
                            },
                            location,
                        )),
                        _ => Err(ProgramError {
                            location,
                            message: "Invalid assignment target".to_owned(),
                        }),
                    }
                } else {
                    Err(ProgramError {
                        location,
                        message: "No right side in assignment".to_owned(),
                    })
                }
            }
            _ => Ok(variable),
        }
    }

    fn parse_ternary(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        let condition = self.parse_or()?;
        if self.peek(TokenType::Question) {
            self.next();
            let then_branch = self.parse_expression()?;
            self.consume(
                TokenType::Colon,
                "Expected ':' after then branch of conditional expression",
                &then_branch.location,
            )?;
            let else_branch = self.parse_ternary()?;
            let location = condition.location.clone();
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::Conditional {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                location,
            ))
        } else {
            Ok(condition)
        }
    }

    fn parse_or(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(Parser::parse_and, Parser::parse_or, &[TokenType::Or])
    }

    fn parse_and(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(Parser::parse_equality, Parser::parse_and, &[TokenType::And])
    }

    fn parse_equality(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(
            Parser::parse_comparison,
            Parser::parse_equality,
            &[TokenType::EqualEqual, TokenType::BangEqual],
        )
    }

    fn parse_comparison(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(
            Parser::parse_addition,
            Parser::parse_comparison,
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
        )
    }

    fn parse_addition(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(
            Parser::parse_multiplication,
            Parser::parse_addition,
            &[TokenType::Minus, TokenType::Plus],
        )
    }

    fn parse_multiplication(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        self.parse_binary(
            Parser::parse_unary,
            Parser::parse_multiplication,
            &[TokenType::Star, TokenType::Slash],
        )
    }

    fn parse_unary(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        if self
            .content
            .borrow_mut()
            .peek()
            .map(|t| [TokenType::Minus, TokenType::Plus].contains(&t.token_type))
            .unwrap_or(false)
        {
            let t = self.next().unwrap();
            let value = self.parse_unary()?;
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::Unary {
                    operator: t.token_type,
                    operand: Box::new(value),
                },
                t.location,
            ))
        } else {
            self.parse_istype()
        }
    }

    fn parse_istype(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        let element = self.parse_call()?;
        if self.peek(TokenType::IsType) {
            let location = self.next().unwrap().location;
            let checked_type = self.parse_checked_type(&location)?;
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::IsType {
                    value: Box::new(element),
                    checked_type,
                },
                location
            ))
        } else {
            Ok(element)
        }
    }

    fn parse_checked_type(&self, location: &SourceCodeLocation<'a>) -> Result<Type<'a>, ProgramError<'a>> {
        match self.next() {
            Some(Token { token_type: TokenType::UppercaseNil, .. }) => Ok(Type::Nil),
            Some(Token { token_type: TokenType::Boolean, .. }) => Ok(Type::Boolean),
            Some(Token { token_type: TokenType::Integer, .. }) => Ok(Type::Integer),
            Some(Token { token_type: TokenType::Float, .. }) => Ok(Type::Float),
            Some(Token { token_type: TokenType::String, .. }) => Ok(Type::String),
            Some(Token { token_type: TokenType::Array, .. }) => Ok(Type::Array),
            Some(Token { token_type: TokenType::Function, .. }) => Ok(Type::Function),
            Some(Token { token_type: TokenType::UppercaseClass, .. }) => Ok(Type::Class),
            Some(Token { token_type: TokenType::UppercaseTrait, .. }) => Ok(Type::Trait),
            Some(Token { token_type: TokenType::Module, .. }) => Ok(Type::Module),
            Some(Token {
                token_type: TokenType::Identifier { name }, location, ..
            }) => {
                Ok(Type::UserDefined(Box::new(
                    self.parse_variable_or_module_access(name, &location)?
                )))
            }
            _ => Err(ProgramError {
                message: "Expected type literal in istype right hand operand".to_owned(),
                location: location.clone(),
            })
        }
    }

    fn parse_call(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        let mut callee = self.parse_array_element()?;
        loop {
            match self.dry_next().map(|t| t.token_type) {
                Some(TokenType::LeftParen) => {
                    callee = self.parse_call_function(callee)?;
                }
                Some(TokenType::Dot) => {
                    callee = self.parse_call_property(callee)?;
                }
                _ => return Ok(callee),
            }
        }
    }

    fn parse_call_property(&self, callee: Expression<'a>) -> Result<Expression<'a>, ProgramError<'a>> {
        self.consume(
            TokenType::Dot,
            "Expected '.' on property call expression",
            &callee.location,
        )?;
        if let Some(TokenType::Identifier { name }) = self.next().map(|t| t.token_type) {
            let location = callee.location.clone();
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::Get {
                    callee: Box::new(callee),
                    property: name.to_owned(),
                },
                location,
            ))
        } else {
            Err(callee.create_program_error("Expected property name after '.'"))
        }
    }

    fn parse_call_function(&self, callee: Expression<'a>) -> Result<Expression<'a>, ProgramError<'a>> {
        self.consume(
            TokenType::LeftParen,
            "Expected '(' on function call expression",
            &callee.location,
        )?;
        let args = self.parse_parameters(&callee.location, Parser::parse_ternary)?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' on function call expression",
            &callee.location,
        )?;
        let location = callee.location.clone();
        Ok(self.expression_factory.borrow_mut().new_expression(
            ExpressionType::Call {
                callee: Box::new(callee),
                arguments: args.into_iter().map(Box::new).collect(),
            },
            location,
        ))
    }

    fn parse_parameters<R, F: Fn(&Parser<'a, I>) -> Result<R, ProgramError<'a>>>(
        &self,
        location: &SourceCodeLocation<'a>,
        parser: F,
    ) -> Result<Vec<R>, ProgramError<'a>> {
        let mut args = vec![];
        loop {
            match self.dry_next() {
                None
                | Some(Token {
                    token_type: TokenType::RightParen,
                    ..
                }) => break,
                _ => {
                    let arg = parser(self)?;
                    args.push(arg);
                    if !self.peek(TokenType::RightParen) {
                        self.consume(
                            TokenType::Comma,
                            "Expected ',' after call argument",
                            location,
                        )?;
                    }
                }
            }
        }
        Ok(args)
    }

    fn parse_identifier(&self) -> Result<String, ProgramError<'a>> {
        match self.next() {
            Some(Token {
                token_type: TokenType::Identifier { name },
                ..
            }) => Ok(name),
            Some(Token { location, token_type, .. }) => Err(ProgramError {
                location,
                message: format!("Expected identifier! Got {:?}", token_type),
            }),
            None => panic!("Can't happen"),
        }
    }

    fn parse_array_element(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        let array = self.parse_primary()?;
        if self.peek(TokenType::LeftSquareBrace) {
            self.next();
            let index = Box::new(self.parse_expression()?);
            self.consume(
                TokenType::RightSquareBrace,
                "Expected `]` at the end of array indexing",
                &index.location,
            )?;
            let location = array.location.clone();
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::ArrayElement {
                    array: Box::new(array),
                    index,
                },
                location,
            ))
        } else {
            Ok(array)
        }
    }

    fn parse_primary(&self) -> Result<Expression<'a>, ProgramError<'a>> {
        match self.next() {
            Some(Token {
                token_type: TokenType::LeftSquareBrace,
                location,
            }) => self.parse_array(location),
            Some(Token {
                token_type: TokenType::Fun,
                location,
            }) => self.parse_anonymous_function(location),
            Some(Token {
                token_type: TokenType::TokenLiteral { value },
                location,
            }) => Ok(self
                .expression_factory
                .borrow_mut()
                .new_expression(ExpressionType::ExpressionLiteral { value }, location)),
            Some(Token {
                token_type: TokenType::Identifier { name },
                location,
            }) => self.parse_variable_or_module_access(name, &location),
            Some(Token {
                token_type: TokenType::LeftParen,
                location,
            }) => self.parse_group(location),
            None => Err(ProgramError {
                message: "Unexpected end of file! Expecting primary".to_owned(),
                location: SourceCodeLocation {
                    file: "",
                    line: 0,
                },
            }),
            Some(Token {
                token_type,
                location,
            }) => self.parse_left_side_missing(&token_type, &location),
        }
    }

    fn parse_variable_or_module_access(&self, name: String, location: &SourceCodeLocation<'a>) -> Result<Expression<'a>, ProgramError<'a>> {
        if self.peek(TokenType::DoubleColon) {
            self.consume(TokenType::DoubleColon, "Expected `::` on module access", location)?;
            let field = Box::new(self.parse_call()?);
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::ModuleLiteral {
                    module: name,
                    field,
                },
                location.clone(),
            ))
        } else {
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::VariableLiteral { identifier: name },
                location.clone(),
            ))
        }
    }

    fn parse_array(&self, location: SourceCodeLocation<'a>) -> Result<Expression<'a>, ProgramError<'a>> {
        if self.peek(TokenType::RightSquareBrace) {
            self.next();
            return Ok(self
                .expression_factory
                .borrow_mut()
                .new_expression(ExpressionType::Array { elements: vec![] }, location));
        }
        let element = Box::new(self.parse_expression()?);
        match self.next() {
            Some(Token {
                token_type: TokenType::Semicolon,
                ..
            }) => {
                let length = Box::new(self.parse_expression()?);
                self.consume(
                    TokenType::RightSquareBrace,
                    "Expected `]` at the end of array literal",
                    &length.location,
                )?;
                Ok(self.expression_factory.borrow_mut().new_expression(
                    ExpressionType::RepeatedElementArray { element, length },
                    location,
                ))
            }
            Some(Token {
                token_type: TokenType::Comma,
                ..
            }) => {
                let mut last_location = element.location.clone();
                let mut elements = vec![element, Box::new(self.parse_expression()?)];
                while !self.peek(TokenType::RightSquareBrace) {
                    self.consume(
                        TokenType::Comma,
                        "Expected `,` between array elements",
                        &last_location,
                    )?;
                    let last_element = Box::new(self.parse_expression()?);
                    last_location = last_element.location.clone();
                    elements.push(last_element);
                }
                self.consume(
                    TokenType::RightSquareBrace,
                    "Expected `]` at the end of array literal",
                    &last_location,
                )?;
                Ok(self
                    .expression_factory
                    .borrow_mut()
                    .new_expression(ExpressionType::Array { elements }, location))
            }
            Some(Token {
                token_type: TokenType::RightSquareBrace,
                ..
            }) => Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::Array {
                    elements: vec![element],
                },
                location,
            )),
            Some(Token {
                location, token_type,
            }) => Err(ProgramError {
                message: format!("Unexpected token `{:?}`", token_type),
                location,
            }),
            None => Err(ProgramError {
                message: "Unexpected end of file".to_owned(),
                location,
            }),
        }
    }

    fn parse_left_side_missing(
        &self,
        token_type: &TokenType,
        location: &SourceCodeLocation<'a>,
    ) -> Result<Expression<'a>, ProgramError<'a>> {
        match token_type {
            TokenType::EqualEqual | TokenType::BangEqual => {
                self.parse_equality()?;
                Err(ProgramError {
                    location: location.clone(),
                    message: "Equality without left side".to_owned(),
                })
            }
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                self.parse_comparison()?;
                Err(ProgramError {
                    location: location.clone(),
                    message: "Comparision without left side".to_owned(),
                })
            }
            TokenType::Plus => {
                self.parse_addition()?;
                Err(ProgramError {
                    location: location.clone(),
                    message: "Addition without left side".to_owned(),
                })
            }
            TokenType::Slash | TokenType::Star => {
                self.parse_multiplication()?;
                Err(ProgramError {
                    location: location.clone(),
                    message: "Multiplication without left side".to_owned(),
                })
            }
            _ => Err(ProgramError {
                location: location.clone(),
                message: format!("Expecting a literal, but got {:?}!", token_type),
            }),
        }
    }

    fn parse_group(&self, location: SourceCodeLocation<'a>) -> Result<Expression<'a>, ProgramError<'a>> {
        let expression = Box::new(self.parse_expression()?);
        self.consume(TokenType::RightParen, "Missing `)`", &location)?;
        Ok(self
            .expression_factory
            .borrow_mut()
            .new_expression(ExpressionType::Grouping { expression }, location))
    }

    fn parse_binary<
        L: Fn(&Parser<'a, I>) -> Result<Expression<'a>, ProgramError<'a>>,
        R: Fn(&Parser<'a, I>) -> Result<Expression<'a>, ProgramError<'a>>,
    >(
        &self,
        parse_left: L,
        parse_right: R,
        operators: &[TokenType],
    ) -> Result<Expression<'a>, ProgramError<'a>> {
        let left = parse_left(self)?;
        if self
            .dry_next()
            .map(|t| operators.contains(&t.token_type))
            .unwrap_or(false)
        {
            let t = self.next().unwrap();
            let right = parse_right(self)?;
            let location = left.location.clone();
            Ok(self.expression_factory.borrow_mut().new_expression(
                ExpressionType::Binary {
                    left: Box::new(left),
                    operator: t.token_type,
                    right: Box::new(right),
                },
                location,
            ))
        } else {
            Ok(left)
        }
    }

    fn peek(&self, token: TokenType) -> bool {
        if let Some(t) = self.content.borrow_mut().peek() {
            token == t.token_type
        } else {
            false
        }
    }

    fn consume(
        &self,
        token: TokenType,
        message: &str,
        location: &SourceCodeLocation<'a>,
    ) -> Result<(), ProgramError<'a>> {
        match self.next() {
            Some(t) if t.token_type == token => Ok(()),
            Some(t) => Err(ProgramError {
                location: t.location,
                message: message.to_owned(),
            }),
            _ => Err(ProgramError {
                location: location.clone(),
                message: message.to_owned(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::types::ExpressionType::ExpressionLiteral;
    use crate::types::StatementType::VariableDeclaration;
    use crate::types::{
        Expression, ExpressionFactory, ExpressionType, Literal, SourceCodeLocation, Statement,
        StatementType, Token, TokenType,
    };

    fn create_expression<'a>(
        expression_type: ExpressionType<'a>,
        location: SourceCodeLocation<'a>,
    ) -> Expression<'a> {
        let mut factory = ExpressionFactory::new();
        factory.new_expression(expression_type, location)
    }

    fn create_expression_with_id<'a>(
        expression_type: ExpressionType<'a>,
        location: SourceCodeLocation<'a>,
        counter: usize,
    ) -> Expression<'a> {
        let mut factory = ExpressionFactory::new_starting(counter);
        factory.new_expression(expression_type, location)
    }

    fn create_statement_expression<'a>(
        expression_type: ExpressionType<'a>,
        location: SourceCodeLocation<'a>,
        counter: usize,
    ) -> Statement<'a> {
        Statement {
            location: location.clone(),
            statement_type: StatementType::Expression {
                expression: create_expression_with_id(expression_type, location, counter),
            },
        }
    }

    #[test]
    fn parse_literal() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![Token {
            location: location.clone(),
            token_type: TokenType::TokenLiteral {
                value: Literal::Float(1.0),
            },
        }];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression(
                ExpressionType::ExpressionLiteral {
                    value: Literal::Float(1.0),
                },
                location,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_identifier() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![Token {
            location: location.clone(),
            token_type: TokenType::Identifier {
                name: "identifier".to_string(),
            },
        }];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression(
                ExpressionType::VariableLiteral {
                    identifier: "identifier".to_owned(),
                },
                location,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_identifier_group() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Grouping {
                    expression: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                },
                location,
                1,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_identifier_group_without_right_paren() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression();
        assert!(result.is_err());
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_identifier_group_without_right_paren_and_more_content() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression();
        assert!(result.is_err());
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_call_with_no_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![],
                },
                location.clone(),
                1,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_call_with_one_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                        1,
                    ))],
                },
                location.clone(),
                2,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_call_with_multiple_arguments() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Comma,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Call {
                    callee: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    arguments: vec![
                        Box::new(create_expression_with_id(
                            ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location.clone(),
                            1,
                        )),
                        Box::new(create_expression_with_id(
                            ExpressionType::VariableLiteral {
                                identifier: "identifier".to_owned(),
                            },
                            location.clone(),
                            2,
                        )),
                    ],
                },
                location.clone(),
                3,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_unary_with_minus() {
        test_unary(TokenType::Minus);
    }

    #[test]
    fn parse_unary_with_plus() {
        test_unary(TokenType::Plus)
    }

    #[test]
    fn parse_multiplication_with_star() {
        test_binary(TokenType::Star);
    }

    #[test]
    fn parse_multiplication_with_slash() {
        test_binary(TokenType::Star);
    }

    #[test]
    fn parse_addition_with_minus() {
        test_binary(TokenType::Minus);
    }

    #[test]
    fn parse_comparison_with_less_equal() {
        test_binary(TokenType::LessEqual);
    }

    #[test]
    fn parse_comparison_with_less() {
        test_binary(TokenType::Less);
    }

    #[test]
    fn parse_comparison_with_greater_equal() {
        test_binary(TokenType::GreaterEqual);
    }

    #[test]
    fn parse_comparison_with_greater() {
        test_binary(TokenType::Greater);
    }

    #[test]
    fn parse_addition_with_plus() {
        test_binary(TokenType::Plus);
    }

    #[test]
    fn parse_equality_with_equal_equal() {
        test_binary(TokenType::EqualEqual);
    }

    #[test]
    fn parse_equality_with_bang_equal() {
        test_binary(TokenType::BangEqual);
    }

    #[test]
    fn parse_and() {
        test_binary(TokenType::And);
    }

    #[test]
    fn parse_or() {
        test_binary(TokenType::Or);
    }

    #[test]
    fn parse_ternary() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Question,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier1".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Colon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier2".to_string(),
                },
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Conditional {
                    condition: Box::new(create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier".to_owned(),
                        },
                        location.clone(),
                    )),
                    then_branch: Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier1".to_owned(),
                        },
                        location.clone(),
                        1,
                    )),
                    else_branch: Box::new(create_expression_with_id(
                        ExpressionType::VariableLiteral {
                            identifier: "identifier2".to_owned(),
                        },
                        location.clone(),
                        2
                    )),
                },
                location.clone(),
                3,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_assignment() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_string(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Equal,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::VariableAssignment {
                    identifier: "identifier".to_string(),
                    expression: Box::new(create_expression_with_id(
                        ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                        1,
                    ))
                },
                location.clone(),
                2,
            )
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_if() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::If,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::If {
                    condition: create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                    ),
                    then: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: create_expression_with_id(
                                ExpressionType::ExpressionLiteral {
                                    value: Literal::Float(1.0),
                                },
                                location.clone(),
                                1,
                            ),
                        },
                    }),
                    otherwise: None,
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_if_else() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::If,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Else,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::If {
                    condition: create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                    ),
                    then: Box::new(create_statement_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0)
                        },
                        location.clone(),
                        1
                    )),
                    otherwise: Some(Box::new(create_statement_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0)
                        },
                        location.clone(),
                        2
                    ))),
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_var() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::VariableDeclaration {
                    name: "identifier".to_owned(),
                    expression: None,
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_var_with_expression() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Equal,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::VariableDeclaration {
                    name: "identifier".to_owned(),
                    expression: Some(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                    )),
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_block() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::Block {
                    body: vec![
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression(
                                    ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned(),
                                    },
                                    location.clone(),
                                ),
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression_with_id(
                                    ExpressionType::ExpressionLiteral {
                                        value: Literal::Float(1.0),
                                    },
                                    location.clone(),
                                    1,
                                ),
                            }
                        }),
                    ]
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_fun() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::Fun,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::FunctionDeclaration {
                    name: "identifier".to_owned(),
                    arguments: vec!["argument".to_owned()],
                    body: vec![
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression(
                                    ExpressionType::VariableLiteral {
                                        identifier: "identifier".to_owned()
                                    },
                                    location.clone(),
                                ),
                            }
                        }),
                        Box::new(Statement {
                            location: location.clone(),
                            statement_type: StatementType::Expression {
                                expression: create_expression_with_id(
                                    ExpressionType::ExpressionLiteral {
                                        value: Literal::Float(1.0),
                                    },
                                    location.clone(),
                                    1,
                                ),
                            }
                        }),
                    ]
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_while() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::While,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        assert_eq!(
            result,
            Statement {
                statement_type: StatementType::While {
                    condition: create_expression(
                        ExpressionType::VariableLiteral {
                            identifier: "argument".to_owned(),
                        },
                        location.clone(),
                    ),
                    action: Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Block {
                            body: vec![
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: create_expression_with_id(
                                            ExpressionType::VariableLiteral {
                                                identifier: "identifier".to_owned(),
                                            },
                                            location.clone(),
                                            1,
                                        ),
                                    }
                                }),
                                Box::new(Statement {
                                    location: location.clone(),
                                    statement_type: StatementType::Expression {
                                        expression: create_expression_with_id(
                                            ExpressionType::ExpressionLiteral {
                                                value: Literal::Float(1.0),
                                            },
                                            location.clone(),
                                            2,
                                        ),
                                    }
                                }),
                            ],
                        }
                    })
                },
                location: location.clone(),
            }
        );
        assert!(parser.is_empty());
    }

    #[test]
    fn parse_full_for() {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::For,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Var,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "argument".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightParen,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::LeftBrace,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: TokenType::Semicolon,
            },
            Token {
                location: location.clone(),
                token_type: TokenType::RightBrace,
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_statement().unwrap();
        let body_statement = Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: create_expression_with_id(
                                ExpressionType::VariableLiteral {
                                    identifier: "identifier".to_owned(),
                                },
                                location.clone(),
                                2,
                            ),
                        },
                    }),
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: StatementType::Expression {
                            expression: create_expression_with_id(
                                ExpressionType::ExpressionLiteral {
                                    value: Literal::Float(1.0),
                                },
                                location.clone(),
                                3,
                            ),
                        },
                    }),
                ],
            },
        };
        let while_statement = Statement {
            statement_type: StatementType::While {
                condition: create_expression(
                    ExpressionType::VariableLiteral {
                        identifier: "argument".to_owned(),
                    },
                    location.clone(),
                ),
                action: Box::new(Statement {
                    location: location.clone(),
                    statement_type: StatementType::Block {
                        body: vec![
                            Box::new(body_statement),
                            Box::new(Statement {
                                location: location.clone(),
                                statement_type: StatementType::Expression {
                                    expression: create_expression_with_id(
                                        ExpressionType::VariableLiteral {
                                            identifier: "argument".to_owned(),
                                        },
                                        location.clone(),
                                        1,
                                    ),
                                },
                            }),
                        ],
                    },
                }),
            },
            location: location.clone(),
        };
        let for_block = Statement {
            location: location.clone(),
            statement_type: StatementType::Block {
                body: vec![
                    Box::new(Statement {
                        location: location.clone(),
                        statement_type: VariableDeclaration {
                            expression: None,
                            name: "identifier".to_owned(),
                        },
                    }),
                    Box::new(while_statement),
                ],
            },
        };
        assert_eq!(result, for_block);
        assert!(parser.is_empty());
    }

    fn test_binary(token_type: TokenType) {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
            Token {
                location: location.clone(),
                token_type: token_type.clone(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Binary {
                    operator: token_type,
                    left: Box::new(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                    )),
                    right: Box::new(create_expression_with_id(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                        1,
                    )),
                },
                location.clone(),
                2,
            )
        );
        assert!(parser.is_empty());
    }

    fn test_unary(token_type: TokenType) {
        let location = SourceCodeLocation {
            line: 1,
            file: "",
        };
        let input = vec![
            Token {
                location: location.clone(),
                token_type: token_type.clone(),
            },
            Token {
                location: location.clone(),
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(1.0),
                },
            },
        ];
        let parser = Parser::new(input.into_iter().peekable());
        let result = parser.parse_expression().unwrap();
        assert_eq!(
            result,
            create_expression_with_id(
                ExpressionType::Unary {
                    operator: token_type,
                    operand: Box::new(create_expression(
                        ExpressionType::ExpressionLiteral {
                            value: Literal::Float(1.0),
                        },
                        location.clone(),
                    )),
                },
                location.clone(),
                1,
            ),
        );
        assert!(parser.is_empty());
    }
}
