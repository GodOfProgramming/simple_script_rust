use crate::expr::{Binary, Expr, Grouping, Literal, Unary, Visitor};
use crate::lex::{Token, TokenType, Value};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        self.expression()
    }

    fn expression<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        self.equality()
    }

    fn equality<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        let mut expr = self.comparision()?;

        while self.match_token(&[TokenType::ExEq, TokenType::EqEq]) {
            let op = self.previous();
            let right = self.comparision()?;
            expr = Box::new(Binary::new(expr, *op, right));
        }

        Ok(expr)
    }

    fn comparision<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        let mut expr = self.addition()?;

        while self.match_token(&[
            TokenType::GreaterThan,
            TokenType::GreaterEq,
            TokenType::LessThan,
            TokenType::LessEq,
        ]) {
            let op = self.previous();
            let right = self.addition()?;
            expr = Box::new(Binary::new(expr, *op, right));
        }

        Ok(expr)
    }

    fn addition<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        let mut expr = self.multiplication()?;

        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let op = self.previous();
            let right = self.multiplication()?;
            expr = Box::new(Binary::new(expr, *op, right));
        }

        Ok(expr)
    }

    fn multiplication<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        let mut expr = self.unary()?;

        while self.match_token(&[TokenType::Slash, TokenType::Asterisk]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Box::new(Binary::new(expr, *op, right));
        }

        Ok(expr)
    }

    fn unary<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        if self.match_token(&[TokenType::Exclamation, TokenType::Minus]) {
            let op = self.previous();
            let right = self.unary()?;
            Ok(Box::new(Unary::new(*op, right)))
        } else {
            self.primary()
        }
    }

    fn primary<T: 'static>(&mut self) -> Result<Box<dyn Expr<'static, T>>, String> {
        if self.match_token(&[
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
            TokenType::NumberLiteral,
            TokenType::StringLiteral,
        ]) {
            let prev = self.previous();

            if let Some(v) = &prev.literal {
                return Ok(Box::new(Literal::new(*v)));
            }
        }

        if self.match_token(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.");
            return Ok(Box::new(Grouping::new(expr)));
        }

        // TODO proper error handling
        Err(format!(""))
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types.iter() {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, t: &TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == *t
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, token_type: &TokenType, msg: &'static str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(format!("{}", msg))
        }
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class => return,
                TokenType::Fun => return,
                TokenType::Var => return,
                TokenType::For => return,
                TokenType::If => return,
                TokenType::While => return,
                TokenType::Print => return,
                TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }
}

pub struct Printer;

impl Printer {
    pub fn new() -> Printer {
        Printer {}
    }

    pub fn print(&mut self, expr: Box<dyn Expr<String>>) -> String {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: &String, exprs: &[&Box<dyn Expr<String>>]) -> String {
        let mut strings = Vec::new();
        strings.push(String::from("("));
        strings.push(name.clone());
        for expr in exprs.iter() {
            strings.push(String::from(" "));
            strings.push(expr.accept(self));
        }
        strings.push(String::from(")"));
        strings.join("")
    }
}

impl Visitor<'_, String> for Printer {
    fn visit_binary_expr(&mut self, e: &Binary<String>) -> String {
        if let Some(lexeme) = &e.operator.lexeme {
            self.parenthesize(&lexeme, &[&e.left, &e.right])
        } else {
            String::from("?")
        }
    }

    fn visit_grouping_expr(&mut self, e: &Grouping<String>) -> String {
        self.parenthesize(&String::from("group"), &[&e.expression])
    }

    fn visit_literal_expr(&mut self, e: &Literal<String>) -> String {
        match &e.value {
            Value::Str(s) => s.clone(),
            Value::Num(f) => format!("{}", f),
        }
    }

    fn visit_unary_expr(&mut self, e: &Unary<String>) -> String {
        if let Some(lexeme) = &e.operator.lexeme {
            self.parenthesize(&lexeme, &[&e.right])
        } else {
            String::from("?")
        }
    }
}
