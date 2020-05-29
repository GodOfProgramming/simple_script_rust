use crate::expr::Expr;
use crate::lex::Token;

pub enum Stmt {
    Block(Box<BlockStmt>),
    Expression(Box<ExpressionStmt>),
    Print(Box<PrintStmt>),
    Var(Box<VarStmt>),
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Stmt::Block(x) => visitor.visit_block_stmt(x),
            Stmt::Expression(x) => visitor.visit_expression_stmt(x),
            Stmt::Print(x) => visitor.visit_print_stmt(x),
            Stmt::Var(x) => visitor.visit_var_stmt(x),
        }
    }
}

pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> BlockStmt {
        BlockStmt { statements }
    }
}

pub struct ExpressionStmt {
    pub expr: Expr,
}

impl ExpressionStmt {
    pub fn new(expr: Expr) -> ExpressionStmt {
        ExpressionStmt { expr }
    }
}

pub struct PrintStmt {
    pub expr: Expr,
}

impl PrintStmt {
    pub fn new(expr: Expr) -> PrintStmt {
        PrintStmt { expr }
    }
}

pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Expr>) -> VarStmt {
        VarStmt { name, initializer }
    }
}

pub trait Visitor<R> {
    fn visit_block_stmt(&mut self, e: &BlockStmt) -> R;
    fn visit_expression_stmt(&mut self, e: &ExpressionStmt) -> R;
    fn visit_print_stmt(&mut self, e: &PrintStmt) -> R;
    fn visit_var_stmt(&mut self, e: &VarStmt) -> R;
}
