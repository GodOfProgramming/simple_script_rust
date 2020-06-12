use crate::expr::Expr;
use crate::lex::Token;
use std::rc::Rc;

pub enum Stmt {
    Return(Box<ReturnStmt>),
    Function(Box<FunctionStmt>),
    While(Box<WhileStmt>),
    If(Box<IfStmt>),
    Block(Box<BlockStmt>),
    Expression(Box<ExpressionStmt>),
    Print(Box<PrintStmt>),
    Var(Box<VarStmt>),
}

pub fn accept<R>(e: Stmt, visitor: &mut dyn Visitor<R>) -> R {
    match e {
        Stmt::Return(x) => visitor.visit_return_stmt(x),
        Stmt::Function(x) => visitor.visit_function_stmt(x),
        Stmt::While(x) => visitor.visit_while_stmt(x),
        Stmt::If(x) => visitor.visit_if_stmt(x),
        Stmt::Block(x) => visitor.visit_block_stmt(x),
        Stmt::Expression(x) => visitor.visit_expression_stmt(x),
        Stmt::Print(x) => visitor.visit_print_stmt(x),
        Stmt::Var(x) => visitor.visit_var_stmt(x),
    }
}

pub fn accept_ref<R>(e: &Stmt, visitor: &mut dyn Visitor<R>) -> R {
    match e {
        Stmt::Return(x) => visitor.visit_return_stmt_ref(x),
        Stmt::Function(x) => visitor.visit_function_stmt_ref(x),
        Stmt::While(x) => visitor.visit_while_stmt_ref(x),
        Stmt::If(x) => visitor.visit_if_stmt_ref(x),
        Stmt::Block(x) => visitor.visit_block_stmt_ref(x),
        Stmt::Expression(x) => visitor.visit_expression_stmt_ref(x),
        Stmt::Print(x) => visitor.visit_print_stmt_ref(x),
        Stmt::Var(x) => visitor.visit_var_stmt_ref(x),
    }
}

pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(keyword: Token, value: Option<Expr>) -> Self {
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
    pub condition: Expr,
    pub body: Stmt,
}

impl WhileStmt {
    pub fn new(token: Token, condition: Expr, body: Stmt) -> Self {
        Self {
            token,
            condition,
            body,
        }
    }
}

pub struct IfStmt {
    pub condition: Expr,
    pub if_true: Stmt,
    pub if_false: Option<Stmt>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: Stmt, if_false: Option<Stmt>) -> Self {
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
    pub expr: Expr,
}

impl ExpressionStmt {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

pub struct PrintStmt {
    pub expr: Expr,
}

impl PrintStmt {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

pub trait Visitor<R> {
    fn visit_return_stmt(&mut self, e: Box<ReturnStmt>) -> R;
    fn visit_return_stmt_ref(&mut self, e: &ReturnStmt) -> R;
    fn visit_function_stmt(&mut self, e: Box<FunctionStmt>) -> R;
    fn visit_function_stmt_ref(&mut self, e: &FunctionStmt) -> R;
    fn visit_while_stmt(&mut self, e: Box<WhileStmt>) -> R;
    fn visit_while_stmt_ref(&mut self, e: &WhileStmt) -> R;
    fn visit_if_stmt(&mut self, e: Box<IfStmt>) -> R;
    fn visit_if_stmt_ref(&mut self, e: &IfStmt) -> R;
    fn visit_block_stmt(&mut self, e: Box<BlockStmt>) -> R;
    fn visit_block_stmt_ref(&mut self, e: &BlockStmt) -> R;
    fn visit_expression_stmt(&mut self, e: Box<ExpressionStmt>) -> R;
    fn visit_expression_stmt_ref(&mut self, e: &ExpressionStmt) -> R;
    fn visit_print_stmt(&mut self, e: Box<PrintStmt>) -> R;
    fn visit_print_stmt_ref(&mut self, e: &PrintStmt) -> R;
    fn visit_var_stmt(&mut self, e: Box<VarStmt>) -> R;
    fn visit_var_stmt_ref(&mut self, e: &VarStmt) -> R;
}
