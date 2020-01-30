use crate::types::{DataKeyword, Literal, ProgramError, SourceCodeLocation, Token, TokenType};
use std::str::FromStr;

pub struct Lexer<'a> {
    content: Vec<char>,
    current: usize,
    file: &'a str,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(s: String, file: &'a str) -> Lexer<'a> {
        Lexer {
            file,
            content: s.chars().collect(),
            current: 0,
            line: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Token<'a>>, Vec<ProgramError<'a>>> {
        let mut result = vec![];
        let mut errors = vec![];
        while self.current < self.content.len() {
            let lexem = self.content[self.current];
            let next = self.peek();
            let token = match (lexem, next) {
                ('(', _) => Some(self.create_token(TokenType::LeftParen)),
                (')', _) => Some(self.create_token(TokenType::RightParen)),
                ('{', _) => Some(self.create_token(TokenType::LeftBrace)),
                ('}', _) => Some(self.create_token(TokenType::RightBrace)),
                ('[', _) => Some(self.create_token(TokenType::LeftSquareBrace)),
                (']', _) => Some(self.create_token(TokenType::RightSquareBrace)),
                (':', Some(':')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::DoubleColon))
                },
                (':', _) => Some(self.create_token(TokenType::Colon)),
                (',', _) => Some(self.create_token(TokenType::Comma)),
                ('.', _) => Some(self.create_token(TokenType::Dot)),
                ('-', _) => Some(self.create_token(TokenType::Minus)),
                ('+', _) => Some(self.create_token(TokenType::Plus)),
                (';', _) => Some(self.create_token(TokenType::Semicolon)),
                ('/', Some('/')) => {
                    self.current += 1;
                    self.take_while(|c| c != '\n');
                    Some(self.create_token(TokenType::Comment))
                }
                ('/', _) => Some(self.create_token(TokenType::Slash)),
                ('*', _) => Some(self.create_token(TokenType::Star)),
                ('?', _) => Some(self.create_token(TokenType::Question)),
                ('!', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::BangEqual))
                }
                ('!', _) => Some(self.create_token(TokenType::Bang)),
                ('=', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::EqualEqual))
                }
                ('=', Some('>')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::Arrow))
                }
                ('=', _) => Some(self.create_token(TokenType::Equal)),
                ('>', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::GreaterEqual))
                }
                ('>', _) => Some(self.create_token(TokenType::Greater)),
                ('<', Some('=')) => {
                    self.current += 1;
                    Some(self.create_token(TokenType::LessEqual))
                }
                ('<', _) => Some(self.create_token(TokenType::Less)),
                ('\"', _) => {
                    let string_content = self.take_while(|s| s != '\"');
                    self.current += 1;
                    if self.current >= self.content.len() {
                        errors.push(self.create_error("Expected '\"', got end of string"));
                        None
                    } else {
                        Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::QuotedString(string_content),
                            },
                        ))
                    }
                }
                (d, _) if d.is_digit(10) => {
                    let string_content = format!(
                        "{}{}",
                        d,
                        self.take_while(|s| s.is_digit(10) || s == '.' || s.is_alphabetic()),
                    );
                    if string_content.contains('.') {
                        match f32::from_str(string_content.as_str()) {
                            Ok(n) => Some(self.create_token(
                                TokenType::TokenLiteral {
                                    value: Literal::Float(n),
                                },
                            )),
                            Err(_) => {
                                errors.push(self.create_error(&format!(
                                    "Couldn't parse {} as floating point number",
                                    string_content
                                )));
                                None
                            }
                        }
                    } else {
                        match i64::from_str(string_content.as_str()) {
                            Ok(n) => Some(self.create_token(
                                TokenType::TokenLiteral {
                                    value: Literal::Integer(n),
                                },
                            )),
                            Err(_) => {
                                errors.push(self.create_error(&format!(
                                    "Couldn't parse {} as integer",
                                    string_content
                                )));
                                None
                            }
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
                        "and" => Some(self.create_token(TokenType::And)),
                        "class" => Some(self.create_token(TokenType::Class)),
                        "else" => Some(self.create_token(TokenType::Else)),
                        "fun" => Some(self.create_token(TokenType::Fun)),
                        "for" => Some(self.create_token(TokenType::For)),
                        "break" => Some(self.create_token(TokenType::Break)),
                        "if" => Some(self.create_token(TokenType::If)),
                        "or" => Some(self.create_token(TokenType::Or)),
                        "print" => Some(self.create_token(TokenType::Print)),
                        "return" => Some(self.create_token(TokenType::Return)),
                        "var" => Some(self.create_token(TokenType::Var)),
                        "while" => Some(self.create_token(TokenType::While)),
                        "setter" => Some(self.create_token(TokenType::Setter)),
                        "getter" => Some(self.create_token(TokenType::Getter)),
                        "trait" => Some(self.create_token(TokenType::Trait)),
                        "import" => Some(self.create_token(TokenType::Import)),
                        "match" => Some(self.create_token(TokenType::Match)),
                        "istype" => Some(self.create_token(TokenType::IsType)),
                        "mod" => Some(self.create_token(TokenType::Mod)),
                        "Nil" => Some(self.create_token(TokenType::UppercaseNil)),
                        "Boolean" => Some(self.create_token(TokenType::Boolean)),
                        "Integer" => Some(self.create_token(TokenType::Integer)),
                        "Float" => Some(self.create_token(TokenType::Float)),
                        "String" => Some(self.create_token(TokenType::String)),
                        "Function" => Some(self.create_token(TokenType::Function)),
                        "Class" => Some(self.create_token(TokenType::UppercaseClass)),
                        "Trait" => Some(self.create_token(TokenType::UppercaseTrait)),
                        "Array" => Some(self.create_token(TokenType::Array)),
                        "Module" => Some(self.create_token(TokenType::Module)),
                        "true" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::True),
                            },
                        )),
                        "false" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::False),
                            },
                        )),
                        "nil" => Some(self.create_token(
                            TokenType::TokenLiteral {
                                value: Literal::Keyword(DataKeyword::Nil),
                            },
                        )),
                        _ => Some(self.create_token(
                            TokenType::Identifier {
                                name: string_content,
                            },
                        )),
                    }
                }
                ('\n', _) => {
                    self.line += 1;
                    None
                }
                ('\0', _) => Some(self.create_token(TokenType::EOF)),
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

    fn create_error(&self, message: &str) -> ProgramError<'a> {
        ProgramError {
            location: self.get_current_location(),
            message: message.to_owned(),
        }
    }

    fn create_token(&self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            location: self.get_current_location(),
        }
    }

    fn get_current_location(&self) -> SourceCodeLocation<'a> {
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
        let s = "(){}:,.-+;/!*!=;=;==>>=<<=;and;class;else;fun;for;break;if;or;print?return;;".to_owned() +
            "var;while\n// comment\nidentifier\n\"string\"\n123.123\ntrue;false;nil;setter;getter;" +
            "trait[];import;::;123;=>;match;istype;Nil;Boolean;Integer;Float;String;Function;Class;" +
            "Array;Module;Trait;mod";
        let mut lexer = Lexer::new(s.to_owned(), "file");
        let expected = Ok(vec![
            Token {
                token_type: TokenType::LeftParen,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::RightParen,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::LeftBrace,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::RightBrace,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Colon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Comma,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Dot,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Minus,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Plus,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Slash,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Bang,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Star,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::BangEqual,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Equal,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::EqualEqual,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Greater,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::GreaterEqual,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Less,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::LessEqual,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::And,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Class,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Else,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Fun,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::For,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Break,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::If,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Or,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Print,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Question,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Return,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Var,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::While,
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
            },
            Token {
                token_type: TokenType::Comment,
                location: SourceCodeLocation {
                    file: "file",
                    line: 1,
                },
            },
            Token {
                token_type: TokenType::Identifier {
                    name: "identifier".to_owned(),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 2,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::QuotedString("string".to_owned()),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 3,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Float(123.123),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 4,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::True),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::False),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Keyword(DataKeyword::Nil),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Setter,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Getter,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Trait,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::LeftSquareBrace,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::RightSquareBrace,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5

                    ,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Import,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::DoubleColon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::TokenLiteral {
                    value: Literal::Integer(123),
                },
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Arrow,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Match,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::IsType,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::UppercaseNil,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Boolean,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Integer,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Float,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::String,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Function,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::UppercaseClass,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Array,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Module,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::UppercaseTrait,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
            Token {
                token_type: TokenType::Mod,
                location: SourceCodeLocation {
                    file: "file",
                    line: 5,
                },
            },
        ]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unfinished_string() {
        let text = "var s = \"aasdfsadfsdfsadfasdfsdfaasdf";
        let mut lexer = Lexer::new(text.to_owned(), "file");
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file",
                line: 0,
            },
            message: "Expected \'\"\', got end of string".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unexpected_character() {
        let text = "var s = 123;@";
        let mut lexer = Lexer::new(text.to_owned(), "file");
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file",
                line: 0,
            },
            message: "Unexpected character @".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_unparsable_number() {
        let text = "var s = 123a;";
        let mut lexer = Lexer::new(text.to_owned(), "file");
        let expected = Err(vec![ProgramError {
            location: SourceCodeLocation {
                file: "file",
                line: 0,
            },
            message: "Couldn\'t parse 123a as integer".to_owned(),
        }]);
        assert_eq!(lexer.parse(), expected);
    }

    #[test]
    fn test_lexer_with_more_than_one_error() {
        let text = "var s = 123a;\nvar n = 123;@";
        let mut lexer = Lexer::new(text.to_owned(), "file");
        let expected = Err(vec![
            ProgramError {
                location: SourceCodeLocation {
                    file: "file",
                    line: 0,
                },
                message: "Couldn\'t parse 123a as integer".to_owned(),
            },
            ProgramError {
                location: SourceCodeLocation {
                    file: "file",
                    line: 1,
                },
                message: "Unexpected character @".to_owned(),
            },
        ]);
        assert_eq!(lexer.parse(), expected);
    }
}
