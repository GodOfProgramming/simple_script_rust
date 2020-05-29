use crate::lex::{Token, Value};

pub enum Expr {
    Logical(Box<LogicalExpr>),
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Ternary(Box<TernaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Box<LiteralExpr>),
    Unary(Box<UnaryExpr>),
    Variable(Box<VariableExpr>),
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
        match self {
            Expr::Logical(x) => visitor.visit_logical_expr(x),
            Expr::Assign(x) => visitor.visit_assign_expr(x),
            Expr::Binary(x) => visitor.visit_binary_expr(x),
            Expr::Ternary(x) => visitor.visit_ternary_expr(x),
            Expr::Grouping(x) => visitor.visit_grouping_expr(x),
            Expr::Literal(x) => visitor.visit_literal_expr(x),
            Expr::Unary(x) => visitor.visit_unary_expr(x),
            Expr::Variable(x) => visitor.visit_variable_expr(x),
        }
    }
}

pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl LogicalExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> LogicalExpr {
        LogicalExpr {
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
    pub fn new(name: Token, value: Expr) -> AssignExpr {
        AssignExpr { name, value }
    }
}

pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> BinaryExpr {
        BinaryExpr {
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
    pub fn new(condition: Expr, if_true: Expr, if_false: Expr) -> TernaryExpr {
        TernaryExpr {
            condition,
            if_true,
            if_false,
        }
    }
}

pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> GroupingExpr {
        GroupingExpr { expression }
    }
}

pub struct LiteralExpr {
    pub value: Value,
}

impl LiteralExpr {
    pub fn new(value: Value) -> LiteralExpr {
        LiteralExpr { value }
    }
}

pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> UnaryExpr {
        UnaryExpr { operator, right }
    }
}

pub struct VariableExpr {
    pub name: Token,
}

impl VariableExpr {
    pub fn new(name: Token) -> VariableExpr {
        VariableExpr { name }
    }
}

pub trait Visitor<R> {
    fn visit_logical_expr(&mut self, e: &LogicalExpr) -> R;
    fn visit_assign_expr(&mut self, e: &AssignExpr) -> R;
    fn visit_binary_expr(&mut self, e: &BinaryExpr) -> R;
    fn visit_ternary_expr(&mut self, e: &TernaryExpr) -> R;
    fn visit_grouping_expr(&mut self, e: &GroupingExpr) -> R;
    fn visit_literal_expr(&mut self, e: &LiteralExpr) -> R;
    fn visit_unary_expr(&mut self, e: &UnaryExpr) -> R;
    fn visit_variable_expr(&mut self, e: &VariableExpr) -> R;
}
