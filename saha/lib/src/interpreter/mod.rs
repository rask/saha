//! Saha AST interpreter
//!
//! The AST interpreter is what makes the magic happen. It walks through a
//! generated abstract syntax tree and manages the global and local symbol
//! tables, while executing application logic.

use noisy_float::prelude::*;

use std::{
    collections::HashMap,
    sync::{Arc, Mutex, MutexGuard}
};

use crate::{
    ast::*,
    source::files::FilePosition,
    types::{
        Value, SahaType,
        objects::{AccessParams, SahaObject},
        functions::{
            SahaFunctionArguments,
            SahaCallable
        }
    },
    symbol_table::InstRef,
    errors::{Error, RuntimeError, ParseError}
};

type AstResult = Result<Value, RuntimeError>;

/// AST visitor takes in an AST and visit all expressions and nodes to reduce
/// them to a single thing: a Saha value.
pub struct AstVisitor<'a> {
    ast: &'a Ast,
    self_ref: Option<InstRef>,
    local_refs: HashMap<String, (SahaType, Value)>
}

impl<'a> AstVisitor<'a> {
    /// Get a new AstVisitor instance for an AST.
    pub fn new(ast: &'a Ast, visit_args: SahaFunctionArguments) -> AstVisitor<'a> {
        let mut inject_local_refs: HashMap<String, (SahaType, Value)> = HashMap::new();
        let mut self_ref: Option<InstRef> = None;

        for (k, v) in visit_args {
            if &k == "self" {
                let instref_src = v.clone();
                self_ref = Some(instref_src.obj.unwrap());
            }

            inject_local_refs.insert(k, (v.kind.clone(), v));
        }

        return AstVisitor {
            ast: ast,
            self_ref: self_ref,
            local_refs: inject_local_refs
        };
    }

    /// Get the class and behavior names an object implements.
    fn get_object_implements(&self, obj: &Value) -> Vec<String> {
        let instref = obj.obj.unwrap();
        let instmutex: Arc<Mutex<Box<dyn SahaObject>>> = crate::SAHA_SYMBOL_TABLE.lock().unwrap().instances.get(&instref).unwrap().clone();

        let inst = instmutex.lock().unwrap();

        let mut impl_list = vec![inst.get_fully_qualified_class_name()];
        let mut beh_impl = inst.get_implements();

        impl_list.append(&mut beh_impl);

        return impl_list;
    }

    fn is_matching_type(&self, expected: &SahaType, value: &Value) -> bool {
        let is_match = match (expected, &value.kind) {
            (SahaType::Bool, SahaType::Bool) => true,
            (SahaType::Str, SahaType::Str) => true,
            (SahaType::Int, SahaType::Int) => true,
            (SahaType::Float, SahaType::Float) => true,
            (SahaType::Name(ref exp_name), SahaType::Name(ref act_name)) => exp_name == act_name,
            (SahaType::Name(exp_name), SahaType::Obj) => {
                let mut allowed: Vec<String> = Vec::new();
                let mut obj_implements: Vec<String> = self.get_object_implements(&value);

                return obj_implements.contains(exp_name);
            },
            _ => false
        };

        return is_match;
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

        if self.is_matching_type(old_type, &value) == false {
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

    /// Get an Arced Mutex to a single saha object instance.
    fn get_instance_lockable_ref(&mut self, instref: &InstRef, access_pos: FilePosition) -> Result<Arc<Mutex<Box<dyn SahaObject>>>, RuntimeError> {
        let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

        if st.instances.contains_key(instref) == false {
            let err = RuntimeError::new(
                "Cannot access undefined instance",
                Some(access_pos)
            );

            return Err(err.with_type("KeyError"));
        }

        return Ok(st.instances.get(instref).unwrap().clone());
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

            if self.is_matching_type(var_type, &default_value) == false {
                let err = RuntimeError::new(
                    &format!(
                        "Mismatching type assigned to variable `{}`, expected `{:?}` but received `{:?}`",
                        refname,
                        var_type.to_readable_string(),
                        default_value.kind.to_readable_string()
                    ),
                    Some(def_expr.file_position)
                );

                return Err(err.with_type("TypeError"));
            }
        }

        self.create_local_ref(refname, (var_type.to_owned(), default_value), refpos)?;

        return Ok(Value::void());
    }

    /// Visit an expression.
    fn visit_expression(&mut self, expression: &Box<Expression>) -> AstResult {
        match &expression.kind {
            ExpressionKind::LiteralValue(val) => Ok(val.clone()),
            ExpressionKind::BinaryOperation(lhs, op, rhs) => self.visit_binop_expression(lhs, op, rhs),
            ExpressionKind::UnaryOperation(unop, expr) => self.visit_unop(unop, expr),
            ExpressionKind::Assignment(identpath, expr) => self.visit_assignment(identpath, expr),
            ExpressionKind::FunctionCall(identpath, call_args) => self.visit_callable_call(identpath, call_args),
            ExpressionKind::IdentPath(..) => self.resolve_ident_path_to_value(&expression),
            ExpressionKind::NewInstance(ident, args) => self.visit_instance_newup(ident, args),
            _ => unimplemented!("{:?}", expression.kind)
        }
    }

    /// Visit a name assignment node.
    fn visit_assignment(&mut self, ident_path: &Box<Expression>, value_expr: &Box<Expression>) -> AstResult {
        let (owner, access_kind, property) = self.resolve_ident_path(ident_path)?;
        let value = self.visit_expression(value_expr)?;

        if owner.is_none() {
            return self.set_local_ref(property.identifier, value, &property.file_position);
        } else {
            unimplemented!()
        }
    }

    /// Visit and resolve an identifier path expression to a value.
    fn resolve_ident_path_to_value(&mut self, ident_path: &Box<Expression>) -> AstResult {
        let (root, acckind, member) = self.resolve_ident_path(ident_path)?;

        if root.is_none() {
            return self.get_local_ref(member.identifier.clone(), &ident_path.file_position);
        }

        let access_val: Value = self.access_object_property(root.unwrap(), acckind.unwrap_or(AccessKind::Instance), member)?;

        return Ok(access_val);
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
                    &format!("Mismatching operands for operation: `{:?} - {:?}`", lkstr, rkstr),
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
                    &format!("Mismatching operands for operation: `{:?} * {:?}`", lkstr, rkstr),
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

                Value::float(lhs_value.float.unwrap() / rhs_value.float.unwrap())
            },
            (SahaType::Int, SahaType::Int) => {
                if rhs_value.int.unwrap() == 0 {
                    let err = RuntimeError::new("Division by zero", Some(op_pos.clone()));

                    return Err(err.with_type("MathError"));
                }

                Value::int(lhs_value.int.unwrap() / rhs_value.int.unwrap())
            },
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} / {:?}`", lkstr, rkstr),
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
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() > rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.int.unwrap() > rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} > {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_gte(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() >= rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.int.unwrap() >= rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} >= {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_lt(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() < rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.int.unwrap() < rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} < {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_lte(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (lk, rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() <= rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.int.unwrap() <= rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} <= {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err.with_type("TypeError"));
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_and(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        match lhs_value.kind {
            SahaType::Bool => {
                if lhs_value.bool.unwrap() == false {
                    // lhs is false so no point attempting to parse the right side for no reason
                    return Ok(Value::bool(false));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid left operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err.with_type("TypeError"));
            }
        };

        // we got this far because lhs is a true boolean value, lets check the right side

        let rhs_value: Value = self.visit_expression(rhs)?;

        match rhs_value.kind {
            SahaType::Bool => {
                if rhs_value.bool.unwrap() == false {
                    // rhs is false so no matter what lhs was, we return false
                    return Ok(Value::bool(false));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid right operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err.with_type("TypeError"));
            }
        };

        // both true!
        return Ok(Value::bool(true));
    }

    /// Visit binop expression.
    fn visit_binop_or(&mut self, lhs: &Box<Expression>, rhs: &Box<Expression>, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        match lhs_value.kind {
            SahaType::Bool => {
                if lhs_value.bool.unwrap() == true {
                    // lhs is true so no point attempting to parse the right side for no reason
                    return Ok(Value::bool(true));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid left operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err.with_type("TypeError"));
            }
        };

        // we got this far because lhs is a true boolean value, lets check the right side

        let rhs_value: Value = self.visit_expression(rhs)?;

        match rhs_value.kind {
            SahaType::Bool => {
                if rhs_value.bool.unwrap() == true {
                    // rhs is false so no matter what lhs was, we return false
                    return Ok(Value::bool(true));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid right operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err.with_type("TypeError"));
            }
        };

        // both false!
        return Ok(Value::bool(false));
    }

    fn visit_unop(&mut self, unop: &UnaryOp, expr: &Box<Expression>) -> AstResult {
        let expr_value = self.visit_expression(expr)?;

        let new_val: Value = match unop.kind {
            UnaryOpKind::Not => {
                if let SahaType::Bool = expr_value.kind {
                    Value::bool(!expr_value.bool.unwrap())
                } else {
                    let err = RuntimeError::new("Invalid unary negation operand, expected boolean", Some(unop.file_position.clone()));

                    return Err(err.with_type("TypeError"));
                }
            },
            UnaryOpKind::Minus => {
                match expr_value.kind {
                    SahaType::Int => Value::int(-expr_value.int.unwrap()),
                    SahaType::Float => Value::float(-expr_value.float.unwrap()),
                    _ => {
                        let err = RuntimeError::new("Invalid unary minus operand, expected boolean", Some(unop.file_position.clone()));

                        return Err(err.with_type("TypeError"));
                    }
                }
            }
        };

        return Ok(new_val);
    }

    /// Visit a callable call.
    fn visit_callable_call(&mut self, ident_path: &Box<Expression>, args: &Box<Expression>) -> AstResult {
        let (owner, acckind, callable) = self.resolve_ident_path(ident_path)?;

        if owner.is_none() {
            return self.call_function(&callable, args);
        } else {
            // FIXME remove the default access kind and fail better
            return self.call_method(&owner.unwrap(), &acckind.unwrap_or(AccessKind::Instance), &callable, args);
        }
    }

    /// Resolve an identifier path. First tuple member is a instref value
    /// (optional), third is the method or property name.
    fn resolve_ident_path(&mut self, path: &Box<Expression>) -> Result<(Option<Value>, Option<AccessKind>, Identifier), RuntimeError> {
        let mut member: Identifier; // callable name
        let mut owner: Option<Value> = None; // instref
        let mut last_access_kind: Option<AccessKind> = None; // inst or static access?

        match &path.kind {
            ExpressionKind::IdentPath(root, members) => {
                if members.is_empty() {
                    member = root.clone();
                } else {
                    let mut memberpath = members.clone();
                    let mut obj_being_accessed: Option<Value> = Some(self.resolve_local_name(&root)?);

                    loop {
                        let (acckind, mname) = memberpath.remove(0);

                        if memberpath.is_empty() {
                            member = mname;
                            owner = obj_being_accessed;
                            last_access_kind = Some(acckind);
                            break;
                        }

                        obj_being_accessed = Some(self.access_object_property(obj_being_accessed.unwrap(), acckind, mname)?);
                    }
                }
            },
            _ => unreachable!()
        }

        return Ok((owner, last_access_kind, member));
    }

    /// Access an object property and get the value it contains.
    fn access_object_property(&mut self, obj: Value, access_kind: AccessKind, property_name: Identifier) -> AstResult {
        let result_value: Value;
        let inst_lockable: Arc<Mutex<Box<dyn SahaObject>>>;

        match obj.kind {
            SahaType::Obj => (),
            _ => {
                let err = RuntimeError::new(
                    &format!("Attempted to access property `{}` of a non-object value", property_name.identifier),
                    Some(property_name.file_position)
                );

                return Err(err.with_type("KeyError"));
            }
        };

        let inst_lockable = self.get_instance_lockable_ref(&obj.obj.unwrap(), property_name.file_position.clone())?;

        let is_static_access = match access_kind {
            AccessKind::Static => true,
            _ => false
        };

        let instref_ctx = self.self_ref;

        let access = AccessParams {
            is_static_access: is_static_access,
            member_name: &property_name.identifier,
            accessor_instref: &self.self_ref,
            access_file_pos: &Some(property_name.file_position)
        };

        result_value = inst_lockable.lock().unwrap().access_property(access)?;

        return Ok(result_value);
    }

    /// Resolve an identifier name to a local ref table value.
    fn resolve_local_name(&mut self, name: &Identifier) -> AstResult {
        let refvalue: Value = self.get_local_ref(name.identifier.clone(), &name.file_position)?;

        return Ok(refvalue);
    }

    fn parse_callable_args(&mut self, args: &Box<Expression>) -> Result<SahaFunctionArguments, RuntimeError> {
        let mut call_args: SahaFunctionArguments = HashMap::new();

        match &args.kind {
            ExpressionKind::CallableArgs(vargs) => {
                for varg in vargs {
                    match &varg.kind {
                        ExpressionKind::CallableArg(argname, argval) => {
                            call_args.insert(argname.identifier.clone(), self.visit_expression(&argval)?);
                        },
                        _ => unreachable!()
                    };
                }
            },
            _ => unreachable!()
        }

        return Ok(call_args);
    }

    /// Call a global/bare function.
    fn call_function(&mut self, callable: &Identifier, args: &Box<Expression>) -> AstResult {
        let func: Box<dyn SahaCallable>;

        {
            let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            if st.functions.contains_key(&callable.identifier) == false {
                let err = RuntimeError::new(
                    &format!("Cannot call undefined function `{}`", callable.identifier),
                    Some(callable.file_position.clone())
                );

                return Err(err.with_type("KeyError"));
            }

            let funcopt = st.functions.get(&callable.identifier).unwrap();

            func = funcopt.clone();
        }

        let call_args: SahaFunctionArguments = self.parse_callable_args(args)?;

        return func.call(call_args, Some(callable.file_position.clone()));
    }

    /// Call an object method.
    fn call_method(&mut self, obj: &Value, access_kind: &AccessKind, callable: &Identifier, args: &Box<Expression>) -> AstResult {
        match &obj.kind {
            SahaType::Void => {
                let err = RuntimeError::new("Cannot access property or call method on a void value", Some(callable.file_position.clone()));

                return Err(err.with_type("TypeError"));
            },
            SahaType::Obj => {
                let instref: InstRef = obj.obj.unwrap();
                let instopt: Arc<Mutex<Box<dyn SahaObject>>>;
                let mut instance: Box<dyn SahaObject>;

                instopt = self.get_instance_lockable_ref(&obj.obj.unwrap(), callable.file_position.clone())?;

                instance = instopt.lock().unwrap().clone();

                drop(instopt); // drop the locked mutex here to prevent lockups if `self` stuff is
                // access inside the upcoming method call

                // the drop above releases the lock, so we need to validate how to manage data
                // races when it comes to instances

                let call_args: SahaFunctionArguments = self.parse_callable_args(args)?;

                let is_static_call = match access_kind {
                    AccessKind::Instance => false,
                    _ => true
                };

                let access = AccessParams {
                    is_static_access: is_static_call,
                    member_name: &callable.identifier,
                    accessor_instref: &self.self_ref,
                    access_file_pos: &Some(callable.file_position.clone())
                };

                instance.call_member(access, call_args)
            },
            _ => {
                let call_args: SahaFunctionArguments = self.parse_callable_args(args)?;

                obj.call_value_method(&callable.file_position, access_kind, &callable.identifier, &call_args)
            }
        }
    }

    /// Visit a newup expression.
    fn visit_instance_newup(&mut self, ident: &Identifier, args: &Box<Expression>) -> AstResult {
        let newup_args: SahaFunctionArguments = self.parse_callable_args(args)?;
        let inst_val: Value;

        {
            let mut st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            inst_val = st.create_object_instance(ident.identifier.clone(), newup_args, &Some(ident.file_position.clone()))?;
        }

        return Ok(inst_val);
    }
}
