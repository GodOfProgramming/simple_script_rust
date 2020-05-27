use crate::expr::Expr;
use crate::lex::Token;
use std::marker::PhantomData;

pub trait Stmt<'a, R> {
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R;
}

pub struct Expression<'a, V>
where
    V: 'a,
{
    pub expr: Box<dyn Expr<'a, V>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Expression<'a, V>
where
    V: 'a,
{
    pub fn new(expr: Box<dyn Expr<'a, V>>) -> Expression<'a, V> {
        Expression {
            expr,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Stmt<'a, R> for Expression<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_expression_stmt(self)
    }
}

pub struct Print<'a, V>
where
    V: 'a,
{
    pub expr: Box<dyn Expr<'a, V>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Print<'a, V>
where
    V: 'a,
{
    pub fn new(expr: Box<dyn Expr<'a, V>>) -> Print<'a, V> {
        Print {
            expr,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Stmt<'a, R> for Print<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_print_stmt(self)
    }
}

pub struct Var<'a, V>
where
    V: 'a,
{
    pub name: Token,
    pub initializer: Option<Box<dyn Expr<'a, V>>>,
    _phantom_data: PhantomData<&'a V>,
}

impl<'a, V> Var<'a, V>
where
    V: 'a,
{
    pub fn new(name: Token, initializer: Option<Box<dyn Expr<'a, V>>>) -> Var<'a, V> {
        Var {
            name,
            initializer,
            _phantom_data: PhantomData,
        }
    }
}

impl<'a, R> Stmt<'a, R> for Var<'a, R>
where
    R: 'a,
{
    fn accept(&self, visitor: &mut dyn Visitor<'a, R>) -> R {
        visitor.visit_var_stmt(self)
    }
}

pub trait Visitor<'a, R>
where
    R: 'a,
{
    fn visit_expression_stmt(&mut self, e: &Expression<'a, R>) -> R;

    fn visit_print_stmt(&mut self, e: &Print<'a, R>) -> R;

    fn visit_var_stmt(&mut self, e: &Var<'a, R>) -> R;
}
