use crate::types::{DataKeyword, Literal, ProgramError, SourceCodeLocation, Token, TokenType};
use std::str::FromStr;

pub struct Lexer {
    content: Vec<char>,
    current: usize,
    file: String,
    line: usize,
}

impl Lexer {
    pub fn new(s: String, file: String) -> Lexer {
        Lexer {
            file,
            content: s.chars().collect(),
            current: 0,
            line: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Token>, Vec<ProgramError>> {
        let mut result = vec![];
        let mut errors = vec![];
        while self.current < self.content.len() {
            let lexem = self.content[self.current];
            let next = self.peek();
            let token = match (lexem, next) {
                ('(', _) => Some(self.create_token(TokenType::LeftParen, "(")),
                (')', _) => Some(self.create_token(TokenType::RightParen, ")")),
                ('{', _) => Some(self.create_token(TokenType::LeftBrace, "{")),
                ('}', _) => Some(self.create_token(TokenType::RightBrace, "}")),
                ('[', _) => Some(self.create_token(TokenType::LeftSquareBrace, "[")),
                (']', _) => Some(self.create_token(TokenType::RightSquareBrace, "]")),
                (':', _) => Some(self.create_token(TokenType::Colon, ":")),
                (',', _) => Some(self.create_token(TokenType::Comma, ",")),
                ('.', _) => Some(self.create_token(TokenType::Dot, ".")),
                ('-', _) => Some(self.create_token(TokenType::Minus, ")")),
                ('+', _) => Some(self.create_token(TokenType::Plus, "{")),
                (';', _) => Some(self.create_token(TokenType::Semicolon, ";")),
                ('/', Some('/')) => {
                    self.current += 1;
                    let comment = self.take_while(|c| c != '\n');
                    Some(self.create_token(TokenType::Comment, format!("//{}", comment).as_str()))
                }
                ('/', _) => Some(self.create_token(TokenType::Slash, "/")),
                ('*', _) => Some(self.create_token(TokenType::Star, "*")),
                ('?', _) => Some(self.create_token(TokenType::Question, "?")),
                ('!', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::BangEqual, "!="))
                }
                ('!', _) => Some(self.create_token(TokenType::Bang, "!")),
                ('=', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::EqualEqual, "=="))
                }
                ('=', _) => Some(self.create_token(TokenType::Equal, "=")),
                ('>', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::GreaterEqual, ">="))
                }
                ('>', _) => Some(self.create_token(TokenType::Greater, ">")),
                ('<', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::LessEqual, "<="))
                }
                ('<', _) => Some(self.create_token(TokenType::Less, "<")),
                ('\"', _) => {
                    let string_content = self.take_while(|s| s != '\"');
                    self.current += 1;
                    if self.current >= self.content.len() {
                        errors.push(self.create_error("Expected '\"', got end of string"));
                        None
                    } else {
                        //self.current += 1;
                        let lexeme = format!("\"{}\"", string_content);
                        Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::QuotedString(string_content),
                            },
                            lexeme.as_str(),
                        ))
                    }
                }
                (d, _) if d.is_digit(10) => {
                    let string_content = format!(
                        "{}{}",
                        d,
                        self.take_while(|s| s.is_digit(10) || s == '.' || s.is_alphabetic()),
                    );
                    match f32::from_str(string_content.as_str()) {
                        Ok(n) => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Number(n),
                            },
                            &string_content,
                        )),
                        Err(_) => {
                            errors.push(self.create_error(&format!(
                                "Couldn't parse {} as number",
                                string_content
                            )));
                            None
                        }
                    }
                }
                (c, _) if c.is_alphabetic() || c == '_' => {
                    let string_content = format!(
                        "{}{}",
                        c,
                        self.take_while(|s| s.is_digit(10) || s.is_alphabetic() || c == '_'),
                    );
                    match string_content.as_str() {
                        "and" => Some(self.create_token(TokenType::And, "and")),
                        "class" => Some(self.create_token(TokenType::Class, "class")),
                        "else" => Some(self.create_token(TokenType::Else, "else")),
                        "fun" => Some(self.create_token(TokenType::Fun, "fun")),
                        "for" => Some(self.create_token(TokenType::For, "for")),
                        "break" => Some(self.create_token(TokenType::Break, "break")),
                        "if" => Some(self.create_token(TokenType::If, "if")),
                        "or" => Some(self.create_token(TokenType::Or, "or")),
                        "print" => Some(self.create_token(TokenType::Print, "print")),
                        "return" => Some(self.create_token(TokenType::Return, "return")),
                        "var" => Some(self.create_token(TokenType::Var, "var")),
                        "while" => Some(self.create_token(TokenType::While, "while")),
                        "setter" => Some(self.create_token(TokenType::Setter, "setter")),
                        "getter" => Some(self.create_token(TokenType::Getter, "getter")),
                        "trait" => Some(self.create_token(TokenType::Trait, "trait")),
                        "true" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::True),
                            },
                            "true",
                        )),
                        "false" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::False),
                            },
                            "false",
                        )),
                        "nil" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::Nil),
                            },
                            "nil",
                        )),
                        _ => Some(self.create_token(
                            TokenType::Identifier {
                                name: string_content.clone(),
                            },
                            &string_content,
                        )),
                    }
                }
                ('\n', _) => {
                    self.line += 1;
                    None
                }
                ('\0', _) => Some(self.create_token(TokenType::EOF, "\0")),
                (a, _) if a.is_whitespace() => None,
                (c, _) => {
                    errors.push(self.create_error(&format!("Unexpected character {}", c)));
                    None
                }
            };
            if let Some(t) = token {
                result.push(t);
            }
            self.current += 1;
        }
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn create_error(&self, message: &str) -> ProgramError {
        ProgramError {
            location: self.get_current_location(),
            message: message.to_owned(),
        }
    }

    fn create_token(&self, token_type: TokenType, lexeme: &str) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            location: self.get_current_location(),
        }
    }

    fn get_current_location(&self) -> SourceCodeLocation {
        SourceCodeLocation {
            file: self.file.clone(),
            line: self.line,
        }
    }

    fn peek(&self) -> Option<char> {
        let index = self.current + 1;
        self.content.get(index).cloned()
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, f: F) -> String {
        let mut result = String::from("");
        while let Some(next) = self.peek() {
            if !f(next) {
                break;
            }
            self.current += 1;
            result.push(next);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::types::{DataKeyword, Literal, ProgramError, SourceCodeLocation, Token, TokenType};

    #[test]
    fn test_lexer_with_no_error() {
        let s = "(){}:,.-+;/!*!=;=;==>>=<<=;and;class;else;fun;for;break;if;or;print?return;;var;while\n// comment\nidentifier\n\"string\"\n123.123\ntrue;false;nil;setter;getter;trait[]";
        let mut lexer = Lexer::new(s.to_owned(), "file".to_owned());
        let expected = Ok(vec![
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Colon,
                lexeme: ":".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Comma,
                lexeme: ",".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Dot,
                lexeme: ".".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Minus,
                lexeme: ")".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Plus,
                lexeme: "{".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Slash,
                lexeme: "/".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Bang,
                lexeme: "!".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Star,
                lexeme: "*".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::BangEqual,
                lexeme: "!=".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::EqualEqual,
                lexeme: "==".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Greater,
                lexeme: ">".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::GreaterEqual,
                lexeme: ">=".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Less,
                lexeme: "<".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::LessEqual,
                lexeme: "<=".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::And,
                lexeme: "and".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Class,
                lexeme: "class".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Else,
                lexeme: "else".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Fun,
                lexeme: "fun".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::For,
                lexeme: "for".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Break,
                lexeme: "break".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::If,
                lexeme: "if".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Or,
                lexeme: "or".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Print,
                lexeme: "print".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Question,
                lexeme: "?".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Return,
                lexeme: "return".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Var,
                lexeme: "var".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::While,
                lexeme: "while".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Comment,
                lexeme: "// comment".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 1,
                },
            },
            Token {
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                lexeme: "identifier".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 2,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::QuotedString("string".to_owned()),
                },
                lexeme: "\"string\"".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 3,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Number(123.123),
                },
                lexeme: "123.123".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 4,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::True),
                },
                lexeme: "true".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::False),
                },
                lexeme: "false".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::Nil),
                },
                lexeme: "nil".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Setter,
                lexeme: "setter".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Getter,
                lexeme: "getter".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Trait,
                lexeme: "trait".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::LeftSquareBrace,
                lexeme: "[".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::RightSquareBrace,
                lexeme: "]".to_owned(),
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 5,
                },
            },
        ]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unfinished_string() {
        let text = "var s = \"aasdfsadfsdfsadfasdfsdfaasdf";
        let mut lexer = Lexer::new(text.to_owned(), "file".to_owned());
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file".to_owned(),
                line: 0,
            },
            message: "Expected \'\"\', got end of string".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unexpected_character() {
        let text = "var s = 123;@";
        let mut lexer = Lexer::new(text.to_owned(), "file".to_owned());
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file".to_owned(),
                line: 0,
            },
            message: "Unexpected character @".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unparsable_number() {
        let text = "var s = 123a;";
        let mut lexer = Lexer::new(text.to_owned(), "file".to_owned());
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file".to_owned(),
                line: 0,
            },
            message: "Couldn\'t parse 123a as number".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_more_than_one_error() {
        let text = "var s = 123a;\nvar n = 123;@";
        let mut lexer = Lexer::new(text.to_owned(), "file".to_owned());
        let expected = Err(vec![
            ProgramError {
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 0,
                },
                message: "Couldn\'t parse 123a as number".to_owned(),
            },
            ProgramError {
                location: SourceCodeLocation {
                    file: "file".to_owned(),
                    line: 1,
                },
                message: "Unexpected character @".to_owned(),
            },
        ]);
        assert_eq!(lexer.parse(), expected);
    }
}
