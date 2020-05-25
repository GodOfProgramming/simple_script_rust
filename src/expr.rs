use crate::lex::{Token, Value};
use std::marker::PhantomData;

pub trait Expr<R> {
    fn accept(&self, visitor: &mut dyn Visitor<R>) -> R;
}

pub struct Binary<V> {
    pub left: Box<dyn Expr<V>>,
    pub operator: Token,
    pub right: Box<dyn Expr<V>>,
    phantom_data: PhantomData<V>,
}

impl<V> Binary<V> {
    pub fn new(left: Box<dyn Expr<V>>, operator: Token, right: Box<dyn Expr<V>>) -> Binary<V> {
        Binary {
            left,
            operator,
            right,
            phantom_data: PhantomData,
        }
    }
}

impl<R> Expr<R> for Binary<R> {
    fn accept(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_binary_expr(self)
    }
}

pub struct Grouping<V> {
    pub expression: Box<dyn Expr<V>>,
    phantom_data: PhantomData<V>,
}

impl<V> Grouping<V> {
    pub fn new(expression: Box<dyn Expr<V>>) -> Grouping<V> {
        Grouping {
            expression,
            phantom_data: PhantomData,
        }
    }
}

impl<R> Expr<R> for Grouping<R> {
    fn accept(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_grouping_expr(self)
    }
}

pub struct Literal<V> {
    pub value: Value,
    phantom_data: PhantomData<V>,
}

impl<V> Literal<V> {
    pub fn new(value: Value) -> Literal<V> {
        Literal {
            value,
            phantom_data: PhantomData,
        }
    }
}

impl<R> Expr<R> for Literal<R> {
    fn accept(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_literal_expr(self)
    }
}

pub struct Unary<V> {
    pub operator: Token,
    pub right: Box<dyn Expr<V>>,
    phantom_data: PhantomData<V>,
}

impl<V> Unary<V> {
    pub fn new(operator: Token, right: Box<dyn Expr<V>>) -> Unary<V> {
        Unary {
            operator,
            right,
            phantom_data: PhantomData,
        }
    }
}

impl<R> Expr<R> for Unary<R> {
    fn accept(&self, visitor: &mut dyn Visitor<R>) -> R {
        visitor.visit_unary_expr(self)
    }
}

pub trait Visitor<R> {
    fn visit_binary_expr(&mut self, e: &Binary<R>) -> R;

    fn visit_grouping_expr(&mut self, e: &Grouping<R>) -> R;

    fn visit_literal_expr(&mut self, e: &Literal<R>) -> R;

    fn visit_unary_expr(&mut self, e: &Unary<R>) -> R;
}
