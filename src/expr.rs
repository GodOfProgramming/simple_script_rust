use crate::lex::Token;
use crate::stmt::Stmt;
use crate::types::{Value, Visitor};
use std::rc::Rc;

pub enum Expr {
    Closure(ClosureExpr),
    Range(RangeExpr),
    Logical(LogicalExpr),
    Assign(AssignExpr),
    Binary(BinaryExpr),
    Ternary(TernaryExpr),
    Call(CallExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
}

impl Expr {
    pub fn new_closure(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, id: usize) -> Self {
        Self::Closure(ClosureExpr::new(params, body, id))
    }
    pub fn new_range(begin: Box<Expr>, token: Token, end: Box<Expr>, id: usize) -> Self {
        Self::Range(RangeExpr::new(begin, token, end, id))
    }
    pub fn new_logical(left: Box<Expr>, operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self::Logical(LogicalExpr::new(left, operator, right, id))
    }
    pub fn new_assign(name: Token, value: Box<Expr>, id: usize) -> Self {
        Self::Assign(AssignExpr::new(name, value, id))
    }
    pub fn new_binary(left: Box<Expr>, operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self::Binary(BinaryExpr::new(left, operator, right, id))
    }
    pub fn new_ternary(
        condition: Box<Expr>,

        if_true: Box<Expr>,

        if_false: Box<Expr>,
        id: usize,
    ) -> Self {
        Self::Ternary(TernaryExpr::new(condition, if_true, if_false, id))
    }
    pub fn new_call(callee: Box<Expr>, paren: Token, args: Vec<Expr>, id: usize) -> Self {
        Self::Call(CallExpr::new(callee, paren, args, id))
    }
    pub fn new_grouping(expression: Box<Expr>, id: usize) -> Self {
        Self::Grouping(GroupingExpr::new(expression, id))
    }
    pub fn new_literal(value: Value, id: usize) -> Self {
        Self::Literal(LiteralExpr::new(value, id))
    }
    pub fn new_unary(operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self::Unary(UnaryExpr::new(operator, right, id))
    }
    pub fn new_variable(name: Token, id: usize) -> Self {
        Self::Variable(VariableExpr::new(name, id))
    }
}

pub fn accept<V, R>(e: &Expr, visitor: &mut V) -> R
where
    V: Visitor<ClosureExpr, R>
        + Visitor<RangeExpr, R>
        + Visitor<LogicalExpr, R>
        + Visitor<AssignExpr, R>
        + Visitor<BinaryExpr, R>
        + Visitor<TernaryExpr, R>
        + Visitor<CallExpr, R>
        + Visitor<GroupingExpr, R>
        + Visitor<LiteralExpr, R>
        + Visitor<UnaryExpr, R>
        + Visitor<VariableExpr, R>,
{
    match e {
        Expr::Closure(x) => visitor.visit(x),
        Expr::Range(x) => visitor.visit(x),
        Expr::Logical(x) => visitor.visit(x),
        Expr::Assign(x) => visitor.visit(x),
        Expr::Binary(x) => visitor.visit(x),
        Expr::Ternary(x) => visitor.visit(x),
        Expr::Call(x) => visitor.visit(x),
        Expr::Grouping(x) => visitor.visit(x),
        Expr::Literal(x) => visitor.visit(x),
        Expr::Unary(x) => visitor.visit(x),
        Expr::Variable(x) => visitor.visit(x),
    }
}

pub struct ClosureExpr {
    pub params: Rc<Vec<Token>>,
    pub body: Rc<Vec<Stmt>>,
    pub id: usize,
}

impl ClosureExpr {
    pub fn new(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>, id: usize) -> Self {
        Self { params, body, id }
    }
}

pub struct RangeExpr {
    pub begin: Box<Expr>,
    pub token: Token,
    pub end: Box<Expr>,
    pub id: usize,
}

impl RangeExpr {
    pub fn new(begin: Box<Expr>, token: Token, end: Box<Expr>, id: usize) -> Self {
        Self {
            begin,
            token,
            end,
            id,
        }
    }
}

pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub id: usize,
}

impl LogicalExpr {
    pub fn new(left: Box<Expr>, operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self {
            left,
            operator,
            right,
            id,
        }
    }
}

pub struct AssignExpr {
    pub name: Token,
    pub value: Box<Expr>,
    pub id: usize,
}

impl AssignExpr {
    pub fn new(name: Token, value: Box<Expr>, id: usize) -> Self {
        Self { name, value, id }
    }
}

pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub id: usize,
}

impl BinaryExpr {
    pub fn new(left: Box<Expr>, operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self {
            left,
            operator,
            right,
            id,
        }
    }
}

pub struct TernaryExpr {
    pub condition: Box<Expr>,
    pub if_true: Box<Expr>,
    pub if_false: Box<Expr>,
    pub id: usize,
}

impl TernaryExpr {
    pub fn new(condition: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>, id: usize) -> Self {
        Self {
            condition,
            if_true,
            if_false,
            id,
        }
    }
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
    pub id: usize,
}

impl CallExpr {
    pub fn new(callee: Box<Expr>, paren: Token, args: Vec<Expr>, id: usize) -> Self {
        Self {
            callee,
            paren,
            args,
            id,
        }
    }
}

pub struct GroupingExpr {
    pub expression: Box<Expr>,
    pub id: usize,
}

impl GroupingExpr {
    pub fn new(expression: Box<Expr>, id: usize) -> Self {
        Self { expression, id }
    }
}

pub struct LiteralExpr {
    pub value: Value,
    pub id: usize,
}

impl LiteralExpr {
    pub fn new(value: Value, id: usize) -> Self {
        Self { value, id }
    }
}

pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
    pub id: usize,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Box<Expr>, id: usize) -> Self {
        Self {
            operator,
            right,
            id,
        }
    }
}

pub struct VariableExpr {
    pub name: Token,
    pub id: usize,
}

impl VariableExpr {
    pub fn new(name: Token, id: usize) -> Self {
        Self { name, id }
    }
}
