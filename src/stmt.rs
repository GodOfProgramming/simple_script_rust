use crate::expr::Expr;
use crate::lex::Token;
use crate::types::Visitor;
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

pub fn accept<V, R>(e: &Stmt, visitor: &mut V) -> R
where
    V: Visitor<ReturnStmt, R>
        + Visitor<FunctionStmt, R>
        + Visitor<WhileStmt, R>
        + Visitor<IfStmt, R>
        + Visitor<BlockStmt, R>
        + Visitor<ExpressionStmt, R>
        + Visitor<PrintStmt, R>
        + Visitor<LoadStmt, R>
        + Visitor<LoadrStmt, R>
        + Visitor<VarStmt, R>,
{
    match e {
        Stmt::Return(x) => visitor.visit(x),
        Stmt::Function(x) => visitor.visit(x),
        Stmt::While(x) => visitor.visit(x),
        Stmt::If(x) => visitor.visit(x),
        Stmt::Block(x) => visitor.visit(x),
        Stmt::Expression(x) => visitor.visit(x),
        Stmt::Print(x) => visitor.visit(x),
        Stmt::Load(x) => visitor.visit(x),
        Stmt::Loadr(x) => visitor.visit(x),
        Stmt::Var(x) => visitor.visit(x),
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
