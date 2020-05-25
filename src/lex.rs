use std::collections::HashMap;
use std::fmt::{self, Display};
use std::str;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Asterisk,

    // One or two character tokens.
    Exclamation,
    ExEq,
    Equal,
    EqEq,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,

    // Literals.
    Identifier,
    StringLiteral(String),
    NumberLiteral(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Misc
    Comment,

    // Whitespace
    Space,
    CarriageReturn,
    Tab,

    // Line Delimiters
    NewLine,
    Eof,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    lexeme: Option<String>,
    line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: Option<String>, line: usize) -> Token {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token_type {
            TokenType::StringLiteral(string) => write!(f, "{:?}({})", self.token_type, string),
            TokenType::NumberLiteral(number) => write!(f, "{:?}({})", self.token_type, number),
            other => match &self.lexeme {
                Some(lex) => write!(f, "{:?}({})", other, lex),
                None => write!(f, "{:?}", other),
            },
        }
    }
}

fn basic_keywords() -> HashMap<&'static str, TokenType> {
    let mut map = HashMap::new();
    map.insert("and", TokenType::And);
    map.insert("class", TokenType::Class);
    map.insert("else", TokenType::Else);
    map.insert("false", TokenType::False);
    map.insert("for", TokenType::For);
    map.insert("fun", TokenType::Fun);
    map.insert("if", TokenType::If);
    map.insert("nil", TokenType::Nil);
    map.insert("or", TokenType::Or);
    map.insert("print", TokenType::Print);
    map.insert("return", TokenType::Return);
    map.insert("super", TokenType::Super);
    map.insert("this", TokenType::This);
    map.insert("true", TokenType::True);
    map.insert("var", TokenType::Var);
    map.insert("while", TokenType::While);

    map
}

pub struct Lexer {
    keywords: HashMap<&'static str, TokenType>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            keywords: basic_keywords(),
        }
    }

    pub fn analyze(&self, src: &str) -> Result<(usize, Vec<Token>), usize> {
        enum AnalyzeResult {
            HasLexeme,
            NoLexeme,
        };

        let mut tokens = Vec::new();
        let mut line = 0;
        let mut current_pos = 0usize;

        let peek = |buff: &[u8], current_pos| -> Option<char> {
            if current_pos + 1 >= buff.len() {
                None
            } else {
                Some(buff[current_pos + 1] as char)
            }
        };

        let create_token =
            |buff: &[u8], start, end, token_type| -> Result<(TokenType, String), ()> {
                match str::from_utf8(&buff[start..end]) {
                    Ok(string) => Ok((token_type, String::from(string))),
                    Err(_) => Err(()),
                }
            };

        let is_digit = |c: char| -> bool { c >= '0' && c <= '9' };

        let is_alpha =
            |c: char| -> bool { (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' };

        let is_alphanumeric = |c: char| -> bool { is_digit(c) || is_alpha(c) };

        let next_is = |bytes: &[u8], curr_pos, test: char| -> bool {
            if let Some(next) = peek(&bytes, curr_pos) {
                if next == test {
                    return true;
                }
            }

            false
        };

        let bytes = src.as_bytes();
        let len = bytes.len();
        while current_pos < len {
            let start_pos = current_pos;
            let c = bytes[current_pos] as char;
            let token = match c {
                '(' => Ok((TokenType::LeftParen, AnalyzeResult::HasLexeme)),
                ')' => Ok((TokenType::RightParen, AnalyzeResult::HasLexeme)),
                '{' => Ok((TokenType::LeftBrace, AnalyzeResult::HasLexeme)),
                '}' => Ok((TokenType::RightBrace, AnalyzeResult::HasLexeme)),
                ',' => Ok((TokenType::Comma, AnalyzeResult::HasLexeme)),
                '.' => Ok((TokenType::Dot, AnalyzeResult::HasLexeme)),
                '-' => Ok((TokenType::Minus, AnalyzeResult::HasLexeme)),
                '+' => Ok((TokenType::Plus, AnalyzeResult::HasLexeme)),
                ';' => Ok((TokenType::Semicolon, AnalyzeResult::HasLexeme)),
                '*' => Ok((TokenType::Asterisk, AnalyzeResult::HasLexeme)),
                '!' => {
                    if next_is(&bytes, current_pos, '=') {
                        current_pos += 1;
                        Ok((TokenType::ExEq, AnalyzeResult::HasLexeme))
                    } else {
                        Ok((TokenType::Equal, AnalyzeResult::HasLexeme))
                    }
                }
                '=' => {
                    if next_is(&bytes, current_pos, '=') {
                        current_pos += 1;
                        Ok((TokenType::EqEq, AnalyzeResult::HasLexeme))
                    } else {
                        Ok((TokenType::Equal, AnalyzeResult::HasLexeme))
                    }
                }
                '<' => {
                    if next_is(&bytes, current_pos, '=') {
                        current_pos += 1;
                        Ok((TokenType::LessEq, AnalyzeResult::HasLexeme))
                    } else {
                        Ok((TokenType::Equal, AnalyzeResult::HasLexeme))
                    }
                }
                '>' => {
                    if next_is(&bytes, current_pos, '=') {
                        current_pos += 1;
                        Ok((TokenType::GreaterEq, AnalyzeResult::HasLexeme))
                    } else {
                        Ok((TokenType::Equal, AnalyzeResult::HasLexeme))
                    }
                }
                '/' => {
                    // TODO clean this up/make more efficient
                    if next_is(&bytes, current_pos, '/') {
                        current_pos += 1;
                        while let Some(next) = peek(&bytes, current_pos) {
                            if next == '\n' {
                                break;
                            } else {
                                current_pos += 1;
                            }
                        }
                        Ok((TokenType::Comment, AnalyzeResult::HasLexeme))
                    } else {
                        Ok((TokenType::Slash, AnalyzeResult::HasLexeme))
                    }
                }
                '"' => {
                    // TODO clean this up/make more efficient
                    loop {
                        match peek(&bytes, current_pos) {
                            Some(next) => {
                                if next != '"' {
                                    if next == '\n' {
                                        line += 1;
                                    }
                                } else {
                                    break Ok(());
                                }
                            }
                            None => break Err(line),
                        }
                        current_pos += 1;
                    }?;

                    current_pos += 1;

                    match String::from_utf8(bytes[start_pos + 1..current_pos].to_vec()) {
                        Ok(string) => {
                            Ok((TokenType::StringLiteral(string), AnalyzeResult::HasLexeme))
                        }
                        Err(_) => Err(line),
                    }
                }
                ' ' => Ok((TokenType::Space, AnalyzeResult::NoLexeme)),
                '\r' => Ok((TokenType::CarriageReturn, AnalyzeResult::NoLexeme)),
                '\t' => Ok((TokenType::Tab, AnalyzeResult::NoLexeme)),
                '\n' => {
                    line += 1;
                    Ok((TokenType::NewLine, AnalyzeResult::NoLexeme))
                }
                c => {
                    if is_digit(c) {
                        let mut dot_found = false;
                        while let Some(next) = peek(&bytes, current_pos) {
                            if is_digit(next) {
                                current_pos += 1;
                            } else if next == '.' && !dot_found {
                                if let Some(next_next) = peek(&bytes, current_pos + 1) {
                                    if is_digit(next_next) {
                                        current_pos += 2;
                                        dot_found = true;
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }

                        match str::from_utf8(&bytes[start_pos..current_pos + 1]) {
                            Ok(string) => match string.parse() {
                                Ok(num) => {
                                    Ok((TokenType::NumberLiteral(num), AnalyzeResult::HasLexeme))
                                }
                                Err(_) => Err(line),
                            },
                            Err(_) => Err(line),
                        }
                    } else if is_alpha(c) {
                        while let Some(next) = peek(&bytes, current_pos) {
                            if is_alphanumeric(next) {
                                current_pos += 1;
                            } else {
                                break;
                            }
                        }

                        match str::from_utf8(&bytes[start_pos..current_pos + 1]) {
                            Ok(string) => match self.keywords.get(string) {
                                Some(token) => Ok((token.clone(), AnalyzeResult::HasLexeme)),
                                None => Ok((TokenType::Identifier, AnalyzeResult::HasLexeme)),
                            },
                            Err(_) => Err(line),
                        }
                    } else {
                        Err(line)
                    }
                }
            }?;

            current_pos += 1;

            match token.1 {
                AnalyzeResult::HasLexeme => {
                    match create_token(&bytes, start_pos, current_pos, token.0) {
                        Ok(token) => tokens.push(Token::new(token.0, Some(token.1), line)),
                        Err(_) => return Err(line),
                    }
                }
                AnalyzeResult::NoLexeme => {
                    tokens.push(Token::new(token.0, None, line));
                }
            }
        }

        tokens.push(Token::new(TokenType::Eof, None, line));

        Ok((line, tokens))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const good_src: &'static str = r#"var var_1 = "some value";"#;

    #[test]
    fn lexer_analyze_with_no_error_basic() {
        let lexer = Lexer::new();
        let result = lexer.analyze(good_src);

        let expected_tokens = vec![
            Token::new(TokenType::Var, Some(String::from("var")), 0),
            Token::new(TokenType::Space, None, 0),
            Token::new(TokenType::Identifier, Some(String::from("var_1")), 0),
            Token::new(TokenType::Space, None, 0),
            Token::new(TokenType::Equal, Some(String::from("=")), 0),
            Token::new(TokenType::Space, None, 0),
            Token::new(
                TokenType::StringLiteral(String::from("some value")),
                Some(String::from(r#""some value""#)),
                0,
            ),
            Token::new(TokenType::Semicolon, Some(String::from(";")), 0),
            Token::new(TokenType::Eof, None, 0),
        ];

        match result {
            Ok((line, tokens)) => {
                assert_eq!(line, 0);
                assert_eq!(tokens, expected_tokens);
            }
            Err(_) => assert!(false),
        }
    }
}