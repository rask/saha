//! Saha AST interpreter
//!
//! The AST interpreter is what makes the magic happen. It walks through a
//! generated abstract syntax tree and manages the global and local symbol
//! tables, while executing application logic.

use noisy_float::prelude::*;

use crate::{
    ast::*,
    source::files::FilePosition,
    types::{Value, SahaType},
    errors::{Error, RuntimeError}
};

type AstResult = Result<Value, RuntimeError>;

/// AST visitor takes in an AST and visit all expressions and nodes to reduce
/// them to a single thing: a Saha value.
pub struct AstVisitor<'a> {
    ast: &'a Ast
}

impl<'a> AstVisitor<'a> {
    /// Get a new AstVisitor instance for an AST.
    pub fn new(ast: &'a Ast) -> AstVisitor<'a> {
        return AstVisitor {
            ast: ast
        };
    }

    /// Start visiting.
    pub fn start(&self) -> AstResult {
        return self.visit_block(&self.ast.entrypoint);
    }

    /// Visit a curly block.
    fn visit_block(&self, block: &Box<Block>) -> AstResult {
        for s in &block.statements {
            let is_retmatch = match s.kind {
                StatementKind::Return(..) => true,
                _ => false
            };

            let block_value = self.visit_statement(&s)?;

            if is_retmatch {
                // encountered a return statement, break out early
                return Ok(block_value);
            }
        }

        return Ok(Value::void());
    }

    /// Visit a statement.
    fn visit_statement(&self, statement: &Box<Statement>) -> AstResult {
        match &statement.kind {
            StatementKind::Return(expr) => self.visit_expression(&expr),
            _ => unimplemented!()
        }
    }

    /// Visit an expression.
    fn visit_expression(&self, expression: &Box<Expression>) -> AstResult {
        match &expression.kind {
            ExpressionKind::LiteralValue(val) => Ok(val.clone()),
            ExpressionKind::BinaryOperation(lhs, op, rhs) => self.visit_binop_expression(lhs, op, rhs),
            _ => unimplemented!()
        }
    }

    /// Visit a binary operation expression.
    fn visit_binop_expression(&self, lhs_expr: &Box<Expression>, binop: &BinOp, rhs_expr: &Box<Expression>) -> AstResult {
        match binop.kind {
            BinOpKind::Add => self.visit_binop_add(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Sub => self.visit_binop_sub(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Mul => self.visit_binop_mul(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Div => self.visit_binop_div(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Gt => self.visit_binop_gt(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Gte => self.visit_binop_gte(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Lt => self.visit_binop_lt(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Lte => self.visit_binop_lte(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::And => self.visit_binop_and(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Or => self.visit_binop_or(lhs_expr, rhs_expr, &binop.file_position)
        }
    }

    /// Visit binop expression.
    fn visit_binop_add(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() + rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() + rhs_value.int.unwrap()),
            (SahaType::Str, SahaType::Str) => Value::str(format!("{}{}", lhs_value.str.unwrap(), rhs_value.str.unwrap())),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} + {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_sub(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() - rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() - rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} + {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_mul(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() * rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() * rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} + {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_div(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Float, SahaType::Float) => {
                if rhs_value.float.unwrap() == r64(0.0) {
                    let err = RuntimeError::new("Division by zero", Some(op_pos.clone()));

                    return Err(err.with_type("MathError"));
                }

                Value::float(lhs_value.float.unwrap() + rhs_value.float.unwrap())
            },
            (SahaType::Int, SahaType::Int) => {
                if rhs_value.int.unwrap() == 0 {
                    let err = RuntimeError::new("Division by zero", Some(op_pos.clone()));

                    return Err(err.with_type("MathError"));
                }

                Value::int(lhs_value.int.unwrap() + rhs_value.int.unwrap())
            },
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} + {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_gt(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_gte(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_lt(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_lte(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_and(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_or(&self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }
}