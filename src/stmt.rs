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
    Load(LoadStmt),
    Loadr(LoadrStmt),
    Var(VarStmt),
}

impl Stmt {
    pub fn new_return(keyword: Token, value: Option<Expr>) -> Self {
        Self::Return(ReturnStmt::new(keyword, value))
    }
    pub fn new_function(name: Token, params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
        Self::Function(FunctionStmt::new(name, params, body))
    }
    pub fn new_while(token: Token, condition: Expr, body: Vec<Stmt>) -> Self {
        Self::While(WhileStmt::new(token, condition, body))
    }
    pub fn new_if(condition: Expr, if_true: Vec<Stmt>, if_false: Option<Box<Stmt>>) -> Self {
        Self::If(IfStmt::new(condition, if_true, if_false))
    }
    pub fn new_block(statements: Vec<Stmt>) -> Self {
        Self::Block(BlockStmt::new(statements))
    }
    pub fn new_expression(expr: Expr) -> Self {
        Self::Expression(ExpressionStmt::new(expr))
    }
    pub fn new_print(expr: Expr) -> Self {
        Self::Print(PrintStmt::new(expr))
    }
    pub fn new_load(load: Token, path: Expr) -> Self {
        Self::Load(LoadStmt::new(load, path))
    }
    pub fn new_loadr(loadr: Token, path: Expr) -> Self {
        Self::Loadr(LoadrStmt::new(loadr, path))
    }
    pub fn new_var(name: Token, initializer: Option<Expr>) -> Self {
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
        Stmt::Load(x) => visitor.visit_load_stmt(&x),
        Stmt::Loadr(x) => visitor.visit_loadr_stmt(&x),
        Stmt::Var(x) => visitor.visit_var_stmt(&x),
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
    pub body: Vec<Stmt>,
}

impl WhileStmt {
    pub fn new(token: Token, condition: Expr, body: Vec<Stmt>) -> Self {
        Self {
            token,
            condition,
            body,
        }
    }
}

pub struct IfStmt {
    pub condition: Expr,
    pub if_true: Vec<Stmt>,
    pub if_false: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: Vec<Stmt>, if_false: Option<Box<Stmt>>) -> Self {
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

pub struct LoadStmt {
    pub load: Token,
    pub path: Expr,
}

impl LoadStmt {
    pub fn new(load: Token, path: Expr) -> Self {
        Self { load, path }
    }
}

pub struct LoadrStmt {
    pub loadr: Token,
    pub path: Expr,
}

impl LoadrStmt {
    pub fn new(loadr: Token, path: Expr) -> Self {
        Self { loadr, path }
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
    fn visit_return_stmt(&mut self, e: &ReturnStmt) -> R;
    fn visit_function_stmt(&mut self, e: &FunctionStmt) -> R;
    fn visit_while_stmt(&mut self, e: &WhileStmt) -> R;
    fn visit_if_stmt(&mut self, e: &IfStmt) -> R;
    fn visit_block_stmt(&mut self, e: &BlockStmt) -> R;
    fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> R;
    fn visit_print_stmt(&mut self, e: &PrintStmt) -> R;
    fn visit_load_stmt(&mut self, e: &LoadStmt) -> R;
    fn visit_loadr_stmt(&mut self, e: &LoadrStmt) -> R;
    fn visit_var_stmt(&mut self, e: &VarStmt) -> R;
}
