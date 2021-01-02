use crate::code::{
  parsing::{ParseRule, Parser, Precedence},
  scanning::TokenKind,
};

pub static PARSE_RULES: &[ParseRule] = &[
  // LeftParen
  ParseRule {
    prefix: Some(Parser::grouping),
    infix: None,
    precedence: Precedence::None,
  },
  // RightParen
  // LeftBrace
  // RightBrace
  // Comma
  // Minus
  // Plus
  // Slash
  // Asterisk
  // Semicolon
  // BackSlash
  // Conditional
  // Colon
  // Pipe
  // Not
  // NotEq
  // Equal
  // EqEq
  // GreaterThan
  // GreaterEq
  // LessThan
  // LessEq
  // Dot
  // Range
  // Identifier
  // StringLiteral
  // NumberLiteral
  // And
  // Bool
  // Class
  // Else
  // Error
  // False
  // Fn
  // For
  // If
  // Let
  // List
  // Nil
  // Number
  // Or
  // Print
  // Return
  // String
  // True
  // While
  // EOF
];
