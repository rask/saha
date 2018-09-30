//! Saha AST interpreter
//!
//! The AST interpreter is what makes the magic happen. It walks through a
//! generated abstract syntax tree and manages the global and local symbol
//! tables, while executing application logic.

use noisy_float::prelude::*;

use std::collections::HashMap;

use crate::{
    ast::*,
    source::files::FilePosition,
    types::{
        Value, SahaType,
        objects::SahaObject
    },
    errors::{Error, RuntimeError, ParseError}
};

type AstResult = Result<Value, RuntimeError>;

/// AST visitor takes in an AST and visit all expressions and nodes to reduce
/// them to a single thing: a Saha value.
pub struct AstVisitor<'a> {
    ast: &'a Ast,
    local_refs: HashMap<String, (SahaType, Value)>
}

impl<'a> AstVisitor<'a> {
    /// Get a new AstVisitor instance for an AST.
    pub fn new(ast: &'a Ast) -> AstVisitor<'a> {
        return AstVisitor {
            ast: ast,
            local_refs: HashMap::new()
        };
    }

    /// Create a local reference value.
    fn create_local_ref(&mut self, name: String, value: (SahaType, Value), refpos: &FilePosition) -> AstResult {
        if self.local_refs.get(&name).is_some() {
            let err = RuntimeError::new(&format!("Cannot redeclare variable `{}`", name), Some(refpos.clone()));

            return Err(err.with_type("KeyError"));
        }

        self.local_refs.insert(name, value);

        return Ok(Value::void());
    }

    /// Set a local reference value.
    fn set_local_ref(&mut self, name: String, value: Value, refpos: &FilePosition) -> AstResult {
        let old = self.local_refs.get(&name);

        if old.is_none() {
            let err = RuntimeError::new(&format!("Cannot access undefined variable `{}`", name), Some(refpos.clone()));

            return Err(err.with_type("KeyError"));
        }

        let (old_type, old_value) = old.unwrap();

        if old_type != &value.kind {
            let err = RuntimeError::new(
                &format!(
                    "Cannot assign mismatching type to variable `{}`, expected `{:?}` but received `{:?}`",
                    name,
                    old_type,
                    value.kind
                ),
                Some(refpos.clone())
            );

            return Err(err.with_type("TypeError"));
        }

        self.local_refs.insert(name, (old_type.clone(), value));

        return Ok(Value::void());
    }

    /// Get a local reference value.
    fn get_local_ref(&self, name: String, refpos: &FilePosition) -> AstResult {
        let val = self.local_refs.get(&name);

        if val.is_none() {
            let err = RuntimeError::new(&format!("Cannot access undefined variable `{}`", name), Some(refpos.clone()));

            return Err(err.with_type("KeyError"));
        }

        let (_, value) = val.unwrap();

        if value.kind == SahaType::Void {
            let err = RuntimeError::new(&format!("Cannot access uninitialized variable `{}`", name), Some(refpos.clone()));

            return Err(err.with_type("TypeError"));
        }

        return Ok(value.clone());
    }

    /// Start visiting.
    pub fn start(&mut self) -> AstResult {
        return self.visit_block(&self.ast.entrypoint);
    }

    /// Visit a curly block.
    fn visit_block(&mut self, block: &Box<Block>) -> AstResult {
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
    fn visit_statement(&mut self, statement: &Box<Statement>) -> AstResult {
        match &statement.kind {
            StatementKind::Return(expr) => self.visit_expression(&expr),
            StatementKind::VarDeclaration(ident, vartype, vardefault) => self.visit_variable_declaration_statement(ident, vartype, vardefault),
            StatementKind::Expression(expr) => self.visit_expression(&expr),
            _ => unimplemented!("{:?}", statement.kind)
        }
    }

    /// Visit a variable declaration.
    fn visit_variable_declaration_statement(&mut self, var_ident: &Identifier, var_type: &SahaType, var_default: &Option<Box<Expression>>) -> AstResult {
        let refname = var_ident.identifier.to_owned();
        let refpos = &var_ident.file_position;

        let default_value: Value;

        if var_default.is_none() {
            default_value = Value::void();
        } else {
            let def_expr = var_default.clone().unwrap();
            default_value = self.visit_expression(&def_expr)?;
        }

        self.create_local_ref(refname, (var_type.to_owned(), default_value), refpos)?;

        return Ok(Value::void());
    }

    /// Visit an expression.
    fn visit_expression(&mut self, expression: &Box<Expression>) -> AstResult {
        match &expression.kind {
            ExpressionKind::LiteralValue(val) => Ok(val.clone()),
            ExpressionKind::BinaryOperation(lhs, op, rhs) => self.visit_binop_expression(lhs, op, rhs),
            ExpressionKind::FunctionCall(identpath, call_args) => self.visit_callable_call(identpath, call_args),
            _ => unimplemented!()
        }
    }

    /// Visit a binary operation expression.
    fn visit_binop_expression(&mut self, lhs_expr: &Box<Expression>, binop: &BinOp, rhs_expr: &Box<Expression>) -> AstResult {
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
    fn visit_binop_add(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
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
    fn visit_binop_sub(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
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
    fn visit_binop_mul(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
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
    fn visit_binop_div(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
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
    fn visit_binop_gt(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_gte(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_lt(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_lte(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_and(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit binop expression.
    fn visit_binop_or(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        unimplemented!()
    }

    /// Visit a callable call.
    fn visit_callable_call(&mut self, ident_path: &Box<Expression>, args: &Box<Expression>) -> AstResult {
        let (owner, callable) = self.resolve_ident_path(ident_path)?;

        if owner.is_none() {
            return self.call_function(&callable, args);
        } else {
            return self.call_method(&owner.unwrap(), &callable, args);
        }
    }

    /// Resolve an identifier path. First tuple member is a instref value
    /// (optional), second is the callable name.
    fn resolve_ident_path(&mut self, path: &Box<Expression>) -> Result<(Option<Value>, Identifier), RuntimeError> {
        let mut callable: Identifier; // callable name
        let mut owner: Option<Value> = None; // instref

        match &path.kind {
            ExpressionKind::IdentPath(root, members) => {
                if members.is_empty() {
                    callable = root.clone();
                    owner = None;
                } else {
                    unimplemented!();
                    let rootname = root;
                    let mut rootinst = self.resolve_local_name(rootname)?;
                    let mut accesskind: AccessKind;
                    let mut membername: String;

                    for (ak, mn) in members {

                    }
                }
            },
            _ => unreachable!()
        }

        return Ok((owner, callable));
    }

    /// Resolve an identifier name to a local ref table value.
    fn resolve_local_name(&mut self, name: &Identifier) -> AstResult {
        unimplemented!()
    }

    /// Call a global/bare function.
    fn call_function(&mut self, callable: &Identifier, args: &Box<Expression>) -> AstResult {
        unimplemented!()
    }

    /// Call an object method.
    fn call_method(&mut self, obj: &Value, callable: &Identifier, args: &Box<Expression>) -> AstResult {
        unimplemented!()
    }
}