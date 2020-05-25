use crate::expr::{Expr, Binary, Grouping, Literal, Unary, Visitor, Value};

pub struct Printer;

impl Printer {
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

impl Visitor<String> for Printer {
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
            Value::F64(f) => format!("{}", f),
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

