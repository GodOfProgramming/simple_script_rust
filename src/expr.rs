use crate::lex::Token;
use crate::types::Value;

pub enum Expr {
    Range(Box<RangeExpr>),
    Logical(Box<LogicalExpr>),
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Ternary(Box<TernaryExpr>),
    Call(Box<CallExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<UnaryExpr>),
    Variable(Box<VariableExpr>),
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Range(x) => visitor.visit_range_expr(x),
            Expr::Logical(x) => visitor.visit_logical_expr(x),
            Expr::Assign(x) => visitor.visit_assign_expr(x),
            Expr::Binary(x) => visitor.visit_binary_expr(x),
            Expr::Ternary(x) => visitor.visit_ternary_expr(x),
            Expr::Call(x) => visitor.visit_call_expr(x),
            Expr::Grouping(x) => visitor.visit_grouping_expr(x),
            Expr::Literal(x) => visitor.visit_literal_expr(x),
            Expr::Unary(x) => visitor.visit_unary_expr(x),
            Expr::Variable(x) => visitor.visit_variable_expr(x),
        }
    }
}

pub struct RangeExpr {
    pub begin: Expr,
    pub token: Token,
    pub end: Expr,
}

impl RangeExpr {
    pub fn new(begin: Expr, token: Token, end: Expr) -> Self {
        Self { begin, token, end }
    }
}

pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl LogicalExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

pub struct AssignExpr {
    pub name: Token,
    pub value: Expr,
}

impl AssignExpr {
    pub fn new(name: Token, value: Expr) -> Self {
        Self { name, value }
    }
}

pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

pub struct TernaryExpr {
    pub condition: Expr,
    pub if_true: Expr,
    pub if_false: Expr,
}

impl TernaryExpr {
    pub fn new(condition: Expr, if_true: Expr, if_false: Expr) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

pub struct CallExpr {
    pub callee: Expr,
    pub paren: Token,
    pub args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Self {
            callee,
            paren,
            args,
        }
    }
}

pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

pub struct LiteralExpr {
    pub value: Value,
}

impl LiteralExpr {
    pub fn new(value: Value) -> Self {
        Self { value }
    }
}

pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }
}

pub struct VariableExpr {
    pub name: Token,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

pub trait Visitor<R> {
    fn visit_range_expr(&mut self, e: &RangeExpr) -> R;
    fn visit_logical_expr(&mut self, e: &LogicalExpr) -> R;
    fn visit_assign_expr(&mut self, e: &AssignExpr) -> R;
    fn visit_binary_expr(&mut self, e: &BinaryExpr) -> R;
    fn visit_ternary_expr(&mut self, e: &TernaryExpr) -> R;
    fn visit_call_expr(&mut self, e: &CallExpr) -> R;
    fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> R;
    fn visit_literal_expr(&mut self, e: &LiteralExpr) -> R;
    fn visit_unary_expr(&mut self, e: &UnaryExpr) -> R;
    fn visit_variable_expr(&mut self, e: &VariableExpr) -> R;
}
