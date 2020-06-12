use crate::lex::Token;
use crate::stmt::Stmt;
use crate::types::Value;
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
    pub fn new_closure(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
        Self::Closure(ClosureExpr::new(params, body))
    }
    pub fn new_range(begin: Box<Expr>, token: Token, end: Box<Expr>) -> Self {
        Self::Range(RangeExpr::new(begin, token, end))
    }
    pub fn new_logical(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Self {
        Self::Logical(LogicalExpr::new(left, operator, right))
    }
    pub fn new_assign(name: Token, value: Box<Expr>) -> Self {
        Self::Assign(AssignExpr::new(name, value))
    }
    pub fn new_binary(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Self {
        Self::Binary(BinaryExpr::new(left, operator, right))
    }
    pub fn new_ternary(condition: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>) -> Self {
        Self::Ternary(TernaryExpr::new(condition, if_true, if_false))
    }
    pub fn new_call(callee: Box<Expr>, paren: Token, args: Vec<Expr>) -> Self {
        Self::Call(CallExpr::new(callee, paren, args))
    }
    pub fn new_grouping(expression: Box<Expr>) -> Self {
        Self::Grouping(GroupingExpr::new(expression))
    }
    pub fn new_literal(value: Value) -> Self {
        Self::Literal(LiteralExpr::new(value))
    }
    pub fn new_unary(operator: Token, right: Box<Expr>) -> Self {
        Self::Unary(UnaryExpr::new(operator, right))
    }
    pub fn new_variable(name: Token) -> Self {
        Self::Variable(VariableExpr::new(name))
    }
}

pub fn accept<R>(e: Expr, visitor: &mut dyn Visitor<R>) -> R {
    match e {
        Expr::Closure(x) => visitor.visit_closure_expr(x),
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

pub fn accept_ref<R>(e: &Expr, visitor: &mut dyn Visitor<R>) -> R {
    match e {
        Expr::Closure(x) => visitor.visit_closure_expr_ref(&x),
        Expr::Range(x) => visitor.visit_range_expr_ref(&x),
        Expr::Logical(x) => visitor.visit_logical_expr_ref(&x),
        Expr::Assign(x) => visitor.visit_assign_expr_ref(&x),
        Expr::Binary(x) => visitor.visit_binary_expr_ref(&x),
        Expr::Ternary(x) => visitor.visit_ternary_expr_ref(&x),
        Expr::Call(x) => visitor.visit_call_expr_ref(&x),
        Expr::Grouping(x) => visitor.visit_grouping_expr_ref(&x),
        Expr::Literal(x) => visitor.visit_literal_expr_ref(&x),
        Expr::Unary(x) => visitor.visit_unary_expr_ref(&x),
        Expr::Variable(x) => visitor.visit_variable_expr_ref(&x),
    }
}

pub struct ClosureExpr {
    pub params: Rc<Vec<Token>>,
    pub body: Rc<Vec<Stmt>>,
}

impl ClosureExpr {
    pub fn new(params: Rc<Vec<Token>>, body: Rc<Vec<Stmt>>) -> Self {
        Self { params, body }
    }
}

pub struct RangeExpr {
    pub begin: Box<Expr>,
    pub token: Token,
    pub end: Box<Expr>,
}

impl RangeExpr {
    pub fn new(begin: Box<Expr>, token: Token, end: Box<Expr>) -> Self {
        Self { begin, token, end }
    }
}

pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl LogicalExpr {
    pub fn new(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

pub struct AssignExpr {
    pub name: Token,
    pub value: Box<Expr>,
}

impl AssignExpr {
    pub fn new(name: Token, value: Box<Expr>) -> Self {
        Self { name, value }
    }
}

pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(left: Box<Expr>, operator: Token, right: Box<Expr>) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

pub struct TernaryExpr {
    pub condition: Box<Expr>,
    pub if_true: Box<Expr>,
    pub if_false: Box<Expr>,
}

impl TernaryExpr {
    pub fn new(condition: Box<Expr>, if_true: Box<Expr>, if_false: Box<Expr>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: Box<Expr>, paren: Token, args: Vec<Expr>) -> Self {
        Self {
            callee,
            paren,
            args,
        }
    }
}

pub struct GroupingExpr {
    pub expression: Box<Expr>,
}

impl GroupingExpr {
    pub fn new(expression: Box<Expr>) -> Self {
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
    pub right: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Box<Expr>) -> Self {
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
    fn visit_closure_expr(&mut self, e: ClosureExpr) -> R;
    fn visit_closure_expr_ref(&mut self, e: &ClosureExpr) -> R;
    fn visit_range_expr(&mut self, e: RangeExpr) -> R;
    fn visit_range_expr_ref(&mut self, e: &RangeExpr) -> R;
    fn visit_logical_expr(&mut self, e: LogicalExpr) -> R;
    fn visit_logical_expr_ref(&mut self, e: &LogicalExpr) -> R;
    fn visit_assign_expr(&mut self, e: AssignExpr) -> R;
    fn visit_assign_expr_ref(&mut self, e: &AssignExpr) -> R;
    fn visit_binary_expr(&mut self, e: BinaryExpr) -> R;
    fn visit_binary_expr_ref(&mut self, e: &BinaryExpr) -> R;
    fn visit_ternary_expr(&mut self, e: TernaryExpr) -> R;
    fn visit_ternary_expr_ref(&mut self, e: &TernaryExpr) -> R;
    fn visit_call_expr(&mut self, e: CallExpr) -> R;
    fn visit_call_expr_ref(&mut self, e: &CallExpr) -> R;
    fn visit_grouping_expr(&mut self, e: GroupingExpr) -> R;
    fn visit_grouping_expr_ref(&mut self, e: &GroupingExpr) -> R;
    fn visit_literal_expr(&mut self, e: LiteralExpr) -> R;
    fn visit_literal_expr_ref(&mut self, e: &LiteralExpr) -> R;
    fn visit_unary_expr(&mut self, e: UnaryExpr) -> R;
    fn visit_unary_expr_ref(&mut self, e: &UnaryExpr) -> R;
    fn visit_variable_expr(&mut self, e: VariableExpr) -> R;
    fn visit_variable_expr_ref(&mut self, e: &VariableExpr) -> R;
}
