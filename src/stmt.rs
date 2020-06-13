use crate::expr::Expr;
use crate::lex::Token;
use std::rc::Rc;

pub enum Stmt {
    Return(ReturnStmt),
    Function(FunctionStmt),
    While(WhileStmt),
    If(IfStmt),
    Block(BlockStmt),
    Expression(ExpressionStmt),
    Print(PrintStmt),
    Var(VarStmt),
}

impl Stmt {
    pub fn new_return(keyword: Token, value: Option<Box<Expr>>) -> Self {
        Self::Return(ReturnStmt::new(keyword, value))
    }
    pub fn new_function(name: Token, params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
        Self::Function(FunctionStmt::new(name, params, body))
    }
    pub fn new_while(token: Token, condition: Box<Expr>, body: Box<Stmt>) -> Self {
        Self::While(WhileStmt::new(token, condition, body))
    }
    pub fn new_if(condition: Box<Expr>, if_true: Box<Stmt>, if_false: Option<Box<Stmt>>) -> Self {
        Self::If(IfStmt::new(condition, if_true, if_false))
    }
    pub fn new_block(statements: Vec<Stmt>) -> Self {
        Self::Block(BlockStmt::new(statements))
    }
    pub fn new_expression(expr: Box<Expr>) -> Self {
        Self::Expression(ExpressionStmt::new(expr))
    }
    pub fn new_print(expr: Box<Expr>) -> Self {
        Self::Print(PrintStmt::new(expr))
    }
    pub fn new_var(name: Token, initializer: Option<Box<Expr>>) -> Self {
        Self::Var(VarStmt::new(name, initializer))
    }
}

pub fn accept<R>(e: &Stmt, visitor: &mut dyn Visitor<R>) -> R {
    match e {
        Stmt::Return(x) => visitor.visit_return_stmt(&x),
        Stmt::Function(x) => visitor.visit_function_stmt(&x),
        Stmt::While(x) => visitor.visit_while_stmt(&x),
        Stmt::If(x) => visitor.visit_if_stmt(&x),
        Stmt::Block(x) => visitor.visit_block_stmt(&x),
        Stmt::Expression(x) => visitor.visit_expression_stmt(&x),
        Stmt::Print(x) => visitor.visit_print_stmt(&x),
        Stmt::Var(x) => visitor.visit_var_stmt(&x),
    }
}

pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Box<Expr>>,
}

impl ReturnStmt {
    pub fn new(keyword: Token, value: Option<Box<Expr>>) -> Self {
        Self { keyword, value }
    }
}

pub struct FunctionStmt {
    pub name: Token,
    pub params: Rc<Vec<Token>>,
    pub body: Rc<Vec<Stmt>>,
}

impl FunctionStmt {
    pub fn new(name: Token, params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
        Self { name, params, body }
    }
}

pub struct WhileStmt {
    pub token: Token,
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

impl WhileStmt {
    pub fn new(token: Token, condition: Box<Expr>, body: Box<Stmt>) -> Self {
        Self {
            token,
            condition,
            body,
        }
    }
}

pub struct IfStmt {
    pub condition: Box<Expr>,
    pub if_true: Box<Stmt>,
    pub if_false: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn new(condition: Box<Expr>, if_true: Box<Stmt>, if_false: Option<Box<Stmt>>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

pub struct ExpressionStmt {
    pub expr: Box<Expr>,
}

impl ExpressionStmt {
    pub fn new(expr: Box<Expr>) -> Self {
        Self { expr }
    }
}

pub struct PrintStmt {
    pub expr: Box<Expr>,
}

impl PrintStmt {
    pub fn new(expr: Box<Expr>) -> Self {
        Self { expr }
    }
}

pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Box<Expr>>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Box<Expr>>) -> Self {
        Self { name, initializer }
    }
}

pub trait Visitor<R> {
    fn visit_return_stmt(&mut self, e: &ReturnStmt) -> R;
    fn visit_function_stmt(&mut self, e: &FunctionStmt) -> R;
    fn visit_while_stmt(&mut self, e: &WhileStmt) -> R;
    fn visit_if_stmt(&mut self, e: &IfStmt) -> R;
    fn visit_block_stmt(&mut self, e: &BlockStmt) -> R;
    fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> R;
    fn visit_print_stmt(&mut self, e: &PrintStmt) -> R;
    fn visit_var_stmt(&mut self, e: &VarStmt) -> R;
}
