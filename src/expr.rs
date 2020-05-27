use crate::lex::{Token, Value};
use std::marker::PhantomData;

pub trait Expr<'a, R> {
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R;
}

pub struct Binary<'a, V>
where
    V: 'a,
{
    pub left: Box<dyn Expr<'a, V>>,
    pub operator: Token,
    pub right: Box<dyn Expr<'a, V>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Binary<'a, V>
where
    V: 'a,
{
    pub fn new(
        left: Box<dyn Expr<'a, V>>,

        operator: Token,

        right: Box<dyn Expr<'a, V>>,
    ) -> Binary<'a, V> {
        Binary {
            left,
            operator,
            right,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Expr<'a, R> for Binary<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_binary_expr(self)
    }
}

pub struct Grouping<'a, V>
where
    V: 'a,
{
    pub expression: Box<dyn Expr<'a, V>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Grouping<'a, V>
where
    V: 'a,
{
    pub fn new(expression: Box<dyn Expr<'a, V>>) -> Grouping<'a, V> {
        Grouping {
            expression,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Expr<'a, R> for Grouping<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_grouping_expr(self)
    }
}

pub struct Literal<'a, V>
where
    V: 'a,
{
    pub value: Value,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Literal<'a, V>
where
    V: 'a,
{
    pub fn new(value: Value) -> Literal<'a, V> {
        Literal {
            value,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Expr<'a, R> for Literal<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_literal_expr(self)
    }
}

pub struct Unary<'a, V>
where
    V: 'a,
{
    pub operator: Token,
    pub right: Box<dyn Expr<'a, V>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Unary<'a, V>
where
    V: 'a,
{
    pub fn new(operator: Token, right: Box<dyn Expr<'a, V>>) -> Unary<'a, V> {
        Unary {
            operator,
            right,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Expr<'a, R> for Unary<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_unary_expr(self)
    }
}

pub struct Variable<'a, V>
where
    V: 'a,
{
    pub name: Token,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Variable<'a, V>
where
    V: 'a,
{
    pub fn new(name: Token) -> Variable<'a, V> {
        Variable {
            name,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Expr<'a, R> for Variable<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_variable_expr(self)
    }
}

pub trait Visitor<'a, R>
where
    R: 'a,
{
    fn visit_binary_expr(&mut self, e: &Binary<'a, R>) -> R;

    fn visit_grouping_expr(&mut self, e: &Grouping<'a, R>) -> R;

    fn visit_literal_expr(&mut self, e: &Literal<'a, R>) -> R;

    fn visit_unary_expr(&mut self, e: &Unary<'a, R>) -> R;

    fn visit_variable_expr(&mut self, e: &Variable<'a, R>) -> R;
}
