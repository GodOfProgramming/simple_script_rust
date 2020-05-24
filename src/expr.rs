use crate::lex::{Literal, Token};

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn new_binary(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Expr {
        Expr::Binary {
            left,
            operator,
            right,
        }
    }
    pub fn new_grouping(expression: Box<Expr>) -> Expr {
        Expr::Grouping { expression }
    }
    pub fn new_literal(value: Literal) -> Expr {
        Expr::Literal { value }
    }
    pub fn new_unary(operator: Token, right: Box<Expr>) -> Expr {
        Expr::Unary { operator, right }
    }
}
