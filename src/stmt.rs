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
    Class(ClassStmt),
    Expression(ExpressionStmt),
    Print(PrintStmt),
    Load(LoadStmt),
    Loadr(LoadrStmt),
    Let(LetStmt),
}

impl Stmt {
    pub fn new_return(keyword: Token, value: Option<Expr>, id: usize) -> Self {
        Self::Return(ReturnStmt::new(keyword, value, id))
    }
    pub fn new_function(
        name: Token,

        params: Rc<Vec<Token>>,

        body: Rc<Vec<Stmt>>,
        id: usize,
    ) -> Self {
        Self::Function(FunctionStmt::new(name, params, body, id))
    }
    pub fn new_while(token: Token, condition: Expr, body: Vec<Stmt>, id: usize) -> Self {
        Self::While(WhileStmt::new(token, condition, body, id))
    }
    pub fn new_if(
        condition: Expr,

        if_true: Vec<Stmt>,

        if_false: Option<Box<Stmt>>,
        id: usize,
    ) -> Self {
        Self::If(IfStmt::new(condition, if_true, if_false, id))
    }
    pub fn new_block(statements: Vec<Stmt>, id: usize) -> Self {
        Self::Block(BlockStmt::new(statements, id))
    }
    pub fn new_class(name: Token, methods: Vec<Stmt>, id: usize) -> Self {
        Self::Class(ClassStmt::new(name, methods, id))
    }
    pub fn new_expression(expr: Expr, id: usize) -> Self {
        Self::Expression(ExpressionStmt::new(expr, id))
    }
    pub fn new_print(expr: Expr, id: usize) -> Self {
        Self::Print(PrintStmt::new(expr, id))
    }
    pub fn new_load(load: Token, path: Expr, id: usize) -> Self {
        Self::Load(LoadStmt::new(load, path, id))
    }
    pub fn new_loadr(loadr: Token, path: Expr, id: usize) -> Self {
        Self::Loadr(LoadrStmt::new(loadr, path, id))
    }
    pub fn new_let(name: Token, initializer: Option<Expr>, id: usize) -> Self {
        Self::Let(LetStmt::new(name, initializer, id))
    }
}

pub fn accept<V, R>(e: &Stmt, visitor: &mut V) -> R
where
    V: Visitor<ReturnStmt, R>
        + Visitor<FunctionStmt, R>
        + Visitor<WhileStmt, R>
        + Visitor<IfStmt, R>
        + Visitor<BlockStmt, R>
        + Visitor<ClassStmt, R>
        + Visitor<ExpressionStmt, R>
        + Visitor<PrintStmt, R>
        + Visitor<LoadStmt, R>
        + Visitor<LoadrStmt, R>
        + Visitor<LetStmt, R>,
{
    match e {
        Stmt::Return(x) => visitor.visit(x),
        Stmt::Function(x) => visitor.visit(x),
        Stmt::While(x) => visitor.visit(x),
        Stmt::If(x) => visitor.visit(x),
        Stmt::Block(x) => visitor.visit(x),
        Stmt::Class(x) => visitor.visit(x),
        Stmt::Expression(x) => visitor.visit(x),
        Stmt::Print(x) => visitor.visit(x),
        Stmt::Load(x) => visitor.visit(x),
        Stmt::Loadr(x) => visitor.visit(x),
        Stmt::Let(x) => visitor.visit(x),
    }
}

pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
    pub id: usize,
}

impl ReturnStmt {
    pub fn new(keyword: Token, value: Option<Expr>, id: usize) -> Self {
        Self { keyword, value, id }
    }
}

pub struct FunctionStmt {
    pub name: Token,
    pub params: Rc<Vec<Token>>,
    pub body: Rc<Vec<Stmt>>,
    pub id: usize,
}

impl FunctionStmt {
    pub fn new(name: Token, params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, id: usize) -> Self {
        Self {
            name,
            params,
            body,
            id,
        }
    }
}

pub struct WhileStmt {
    pub token: Token,
    pub condition: Expr,
    pub body: Vec<Stmt>,
    pub id: usize,
}

impl WhileStmt {
    pub fn new(token: Token, condition: Expr, body: Vec<Stmt>, id: usize) -> Self {
        Self {
            token,
            condition,
            body,
            id,
        }
    }
}

pub struct IfStmt {
    pub condition: Expr,
    pub if_true: Vec<Stmt>,
    pub if_false: Option<Box<Stmt>>,
    pub id: usize,
}

impl IfStmt {
    pub fn new(
        condition: Expr,

        if_true: Vec<Stmt>,

        if_false: Option<Box<Stmt>>,
        id: usize,
    ) -> Self {
        Self {
            condition,
            if_true,
            if_false,
            id,
        }
    }
}

pub struct BlockStmt {
    pub statements: Vec<Stmt>,
    pub id: usize,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>, id: usize) -> Self {
        Self { statements, id }
    }
}

pub struct ClassStmt {
    pub name: Token,
    pub methods: Vec<Stmt>,
    pub id: usize,
}

impl ClassStmt {
    pub fn new(name: Token, methods: Vec<Stmt>, id: usize) -> Self {
        Self { name, methods, id }
    }
}

pub struct ExpressionStmt {
    pub expr: Expr,
    pub id: usize,
}

impl ExpressionStmt {
    pub fn new(expr: Expr, id: usize) -> Self {
        Self { expr, id }
    }
}

pub struct PrintStmt {
    pub expr: Expr,
    pub id: usize,
}

impl PrintStmt {
    pub fn new(expr: Expr, id: usize) -> Self {
        Self { expr, id }
    }
}

pub struct LoadStmt {
    pub load: Token,
    pub path: Expr,
    pub id: usize,
}

impl LoadStmt {
    pub fn new(load: Token, path: Expr, id: usize) -> Self {
        Self { load, path, id }
    }
}

pub struct LoadrStmt {
    pub loadr: Token,
    pub path: Expr,
    pub id: usize,
}

impl LoadrStmt {
    pub fn new(loadr: Token, path: Expr, id: usize) -> Self {
        Self { loadr, path, id }
    }
}

pub struct LetStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
    pub id: usize,
}

impl LetStmt {
    pub fn new(name: Token, initializer: Option<Expr>, id: usize) -> Self {
        Self {
            name,
            initializer,
            id,
        }
    }
}
