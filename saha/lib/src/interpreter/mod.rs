//! Saha AST interpreter
//!
//! The AST interpreter is what makes the magic happen. It walks through a
//! generated abstract syntax tree and manages the global and local symbol
//! tables, while executing application logic.

use noisy_float::prelude::*;

use std::{
    collections::HashMap,
    sync::{Arc, Mutex}
};

use crate::prelude::*;
use crate::{
    ast::*,
    types::objects::{ClassDefinition, CoreConstructorFn}
};

type AstResult = Result<Value, RuntimeError>;
type BailableAstResult = Result<(Value, bool), RuntimeError>;
type ResolvedIdentPath = (Option<Value>, Option<SahaType>, Option<AccessKind>, Identifier);

/// AST visitor takes in an AST and visit all expressions and nodes to reduce
/// them to a single thing: a Saha value.
pub struct AstVisitor<'a> {
    ast: &'a Ast,
    self_ref: Option<InstRef>,
    local_refs: HashMap<String, (Box<SahaType>, Value)>
}

impl<'a> AstVisitor<'a> {
    /// Get a new AstVisitor instance for an AST.
    pub fn new(ast: &'a Ast, visit_args: SahaFunctionArguments) -> AstVisitor<'a> {
        let mut inject_local_refs: HashMap<String, (Box<SahaType>, Value)> = HashMap::new();
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

    /// Get an Arced Mutex to a single saha object instance.
    fn get_instance_lockable_ref(&self, instref: &InstRef, access_pos: FilePosition) -> Result<Arc<Mutex<Box<dyn SahaObject>>>, RuntimeError> {
        let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

        if !st.instances.contains_key(instref) {
            let err = RuntimeError::new(
                "Cannot access undefined instance",
                Some(access_pos)
            );

            return Err(err);
        }

        return Ok(Arc::clone(&st.instances[instref]));
    }

    /// Get the class and behavior names an object implements.
    fn get_object_implements(&self, obj: &Value) -> Vec<String> {
        let instref = obj.obj.unwrap();
        let instmutex: Arc<Mutex<Box<dyn SahaObject>>> = crate::SAHA_SYMBOL_TABLE.lock().unwrap().instances[&instref].clone();

        let inst = instmutex.lock().unwrap();

        let mut impl_list = vec![inst.get_fully_qualified_class_name()];
        let mut beh_impl = inst.get_implements();

        impl_list.append(&mut beh_impl);

        return impl_list;
    }

    /// Gets the available type parameter slots for a class using class name.
    fn get_class_type_params(&self, class_name: &str) -> Vec<char> {
        let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

        if !st.classes.contains_key(class_name) {
            unimplemented!()
        }

        let classdef = st.classes[class_name].clone();

        return classdef.type_params.clone().into_iter().map(|(c, _)| c).collect();
    }

    fn get_object_named_type(&self, obj: &Value) -> Box<SahaType> {
        let instref = obj.obj.unwrap();
        let instmutex: Arc<Mutex<Box<dyn SahaObject>>> = crate::SAHA_SYMBOL_TABLE.lock().unwrap().instances[&instref].clone();

        let inst = instmutex.lock().unwrap();

        return inst.get_named_type();
    }

    /// Does a value match a type?
    fn is_matching_type(&self, expected: &SahaType, value: &Value) -> bool {
        let is_match = match (expected, *value.kind.clone()) {
            (SahaType::Bool, SahaType::Bool) => true,
            (SahaType::Str, SahaType::Str) => true,
            (SahaType::Int, SahaType::Int) => true,
            (SahaType::Float, SahaType::Float) => true,
            (SahaType::Name(ref exp_name, ref exp_tp), SahaType::Name(ref act_name, ref act_tp)) => exp_name == act_name && exp_tp == act_tp,
            (SahaType::Name(exp_name, exp_tp), SahaType::Obj) => {
                let impl_list = self.get_object_implements(&value);
                let inst_typedname: Box<SahaType> = self.get_object_named_type(&value);

                if !impl_list.contains(exp_name) {
                    return false;
                }

                match *inst_typedname {
                    SahaType::Name(n, tp) => n == *exp_name && tp == *exp_tp,
                    _ => false
                }
            },
            _ => false
        };

        return is_match;
    }

    /// Create a local reference value.
    fn create_local_ref(&mut self, name: String, value: (Box<SahaType>, Value), refpos: &FilePosition) -> AstResult {
        if self.local_refs.get(&name).is_some() {
            let err = RuntimeError::new(&format!("Cannot redeclare variable `{}`", name), Some(refpos.clone()));

            return Err(err);
        }

        self.local_refs.insert(name, value);

        return Ok(Value::void());
    }

    /// Set a local reference value.
    fn set_local_ref(&mut self, name: String, value: Value, refpos: &FilePosition, allow_create: bool) -> AstResult {
        let old = self.local_refs.get(&name);

        if old.is_none() && !allow_create {
            let err = RuntimeError::new(&format!("Cannot access undefined variable `{}`", name), Some(refpos.clone()));

            return Err(err);
        } else if old.is_none() && allow_create {
            return self.create_local_ref(name, (value.kind.clone(), value.clone()), &refpos);
        }

        let (old_type, _) = old.unwrap();

        if !self.is_matching_type(old_type, &value) {
            let err = RuntimeError::new(
                &format!(
                    "Cannot assign mismatching type to variable `{}`, expected `{:?}` but received `{:?}`",
                    name,
                    old_type,
                    value.kind
                ),
                Some(refpos.clone())
            );

            return Err(err);
        }

        self.local_refs.insert(name, (old_type.clone(), value));

        return Ok(Value::void());
    }

    /// Get a local reference value.
    fn get_local_ref(&self, name: &str, refpos: &FilePosition) -> AstResult {
        let val = self.local_refs.get(name);

        if val.is_none() {
            let err = RuntimeError::new(&format!("Cannot access undefined variable `{}`", name), Some(refpos.clone()));

            return Err(err);
        }

        let (_, value) = val.unwrap();

        if value.kind == Box::new(SahaType::Void) {
            let err = RuntimeError::new(&format!("Cannot access uninitialized variable `{}`", name), Some(refpos.clone()));

            return Err(err);
        }

        return Ok(value.clone());
    }

    /// Remove a local named ref.
    fn remove_local_ref(&mut self, name: &str) -> AstResult {
        self.local_refs.remove(name);

        return Ok(Value::void());
    }

    /// Start visiting.
    pub fn start(&mut self) -> AstResult {
        let (ast_res, _) = self.visit_block(&self.ast.entrypoint, HashMap::new())?;

        // FIXME validate return type

        return Ok(ast_res);
    }

    /// Visit a curly block.
    ///
    /// Returns bailable result, meaning the block can be terminated midway in
    /// case a break or return statement is encountered.
    fn visit_block(&mut self, block: &Block, inject: HashMap<String, (Value, FilePosition)>) -> BailableAstResult {
        if !inject.is_empty() {
            for (n, v) in &inject {
                self.set_local_ref(n.clone(), v.0.clone(), &v.1, true)?;
            }
        }

        for s in &block.statements {
            match s.kind {
                StatementKind::Continue => return Ok((Value::void(), false)),
                StatementKind::Break => return Ok((Value::void(), true)),
                _ => ()
            };

            let is_retmatch = match s.kind {
                StatementKind::Return(..) => true,
                _ => false
            };

            let (block_value, block_bail) = self.visit_statement(&s)?;

            if is_retmatch || block_bail {
                // encountered a return statement, break out early
                if !inject.is_empty() {
                    for (n, _) in inject {
                        self.remove_local_ref(&n)?;
                    }
                }

                return Ok((block_value, true));
            }
        }

        if !inject.is_empty() {
            for (n, _) in inject {
                self.remove_local_ref(&n)?;
            }
        }

        return Ok((Value::void(), false));
    }

    /// Visit a statement. Returns a bailable result, meaning breaks and returns
    /// are propagated up the AST tree to make early block terminations
    /// possible.
    fn visit_statement(&mut self, statement: &Statement) -> BailableAstResult {
        let (res, bail) = match &statement.kind {
            StatementKind::Return(expr) => (self.visit_expression(&expr)?, true),
            StatementKind::VarDeclaration(ident, vartype, vardefault) => (self.visit_variable_declaration_statement(ident, vartype, vardefault)?, false),
            StatementKind::Expression(expr) => (self.visit_expression(&expr)?, false),
            StatementKind::If(if_cond, if_block, elifs, else_block) => self.visit_if_statement(if_cond, if_block, elifs, else_block)?,
            StatementKind::Loop(loop_block) => self.visit_loop_statement(loop_block)?,
            StatementKind::Break => (Value::void(), true),
            StatementKind::Continue => (Value::void(), false),
            StatementKind::For(k_name, v_name, iter_expr, block) => self.visit_for_statement(k_name, v_name, iter_expr, block)?
            //_ => unimplemented!("{:?}", statement.kind)
        };

        return Ok((res, bail));
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

            if !self.is_matching_type(var_type, &default_value) {
                let err = RuntimeError::new(
                    &format!(
                        "Mismatching type assigned to variable `{}`, expected `{:?}` but received `{:?}`",
                        refname,
                        var_type.to_readable_string(),
                        default_value.kind.to_readable_string()
                    ),
                    Some(def_expr.file_position)
                );

                return Err(err);
            }
        }

        self.create_local_ref(refname, (Box::new(var_type.to_owned()), default_value), refpos)?;

        return Ok(Value::void());
    }

    /// Visit an if-elseif-else statement. Returns a bailable result, meaning
    /// return and break statements can terminate the if blocks early where
    /// needed.
    fn visit_if_statement(
        &mut self,
        if_cond: &Expression,
        if_block: &Block,
        elifs: &[Box<Statement>],
        else_block: &Option<Box<Block>>
    ) -> BailableAstResult {
        let cond_value = self.visit_expression(if_cond)?;
        let cond_pos = if_cond.file_position.clone();

        let condition_bool: bool = match *cond_value.kind {
            SahaType::Bool => cond_value.bool.unwrap(),
            _ => {
                let err = RuntimeError::new(
                    &format!("Expected boolean value condition, received `{:?}`", cond_value.kind),
                    Some(cond_pos)
                );

                return Err(err);
            }
        };

        if !condition_bool {
            let mut should_break = false;
            let mut bail: bool = false;

            if !elifs.is_empty() {
                for elifstmt in elifs {
                    let (should, _, bail_maybe) = match &elifstmt.kind {
                        StatementKind::If(cond, block, ..) => self.visit_elseif_statement(cond, block)?,
                        _ => unreachable!()
                    };

                    should_break = should || bail_maybe;

                    bail = bail_maybe;

                    if should_break {
                        break;
                    }
                }

                if should_break {
                    // some elseif matched as true, break out
                    return Ok((Value::void(), bail));
                }
            }

            if else_block.is_some() {
                let elseb = else_block.clone().unwrap();
                let (_, bail_maybe) = self.visit_block(&elseb, HashMap::new())?;

                bail = bail_maybe;
            }

            return Ok((Value::void(), bail));
        }

        // we matched true for the if so we enter the block
        let (_, bail) = self.visit_block(if_block, HashMap::new())?;

        return Ok((Value::void(), bail));
    }

    /// Visit an elseif statement. Returns
    /// `(<was cond true>, <block result value>, <should bail>)` which is an
    /// alteration to the bail result, which allows us to check if the elif cond
    /// was matched as true or not.
    fn visit_elseif_statement(
        &mut self,
        cond: &Expression,
        block: &Block
    ) -> Result<(bool, Value, bool), RuntimeError> {
        let cond_value = self.visit_expression(cond)?;
        let cond_pos = cond.file_position.clone();

        let condition_bool: bool = match *cond_value.kind {
            SahaType::Bool => cond_value.bool.unwrap(),
            _ => {
                let err = RuntimeError::new(
                    &format!("Expected boolean value condition, received `{:?}`", cond_value.kind),
                    Some(cond_pos)
                );

                return Err(err);
            }
        };

        if !condition_bool {
            return Ok((false, Value::void(), false));
        }

        // we matched true for the if so we enter the block
        let (_, bail) = self.visit_block(block, HashMap::new())?;

        return Ok((true, Value::void(), bail));
    }

    /// Visit a loop statement.
    fn visit_loop_statement(&mut self, block: &Block) -> BailableAstResult {
        let mut loop_val;

        'lloop: loop {
            let (val, should_break) = self.visit_block(block, HashMap::new())?;

            loop_val = val;

            if should_break {
                break 'lloop;
            }
        }

        return Ok((loop_val, false));
    }

    /// Visit a for loop.
    fn visit_for_statement(&mut self, k_name: &Identifier, v_name: &Identifier, iterable_expr: &Expression, for_block: &Block) -> BailableAstResult {
        let iterable = self.visit_expression(iterable_expr)?;

        match *iterable.kind {
            SahaType::Obj => {
                let iterable_impl = self.get_object_implements(&iterable);

                if !iterable_impl.contains(&"List".to_string()) && !iterable_impl.contains(&"Dict".to_string()) {
                    let err = RuntimeError::new("Cannot loop over a non-iterable value", Some(iterable_expr.file_position.clone()));

                    return Err(err);
                }
            },
            _ => {
                let err = RuntimeError::new("Cannot loop over a non-iterable value", Some(iterable_expr.file_position.clone()));

                return Err(err);
            }
        };

        return self.visit_for_iterable_statement(k_name, v_name, &iterable, for_block);
    }

    /// Visit a for loop over a List.
    fn visit_for_iterable_statement(&mut self, k_name: &Identifier, v_name: &Identifier, iterable: &Value, for_block: &Block) -> BailableAstResult {
        let inst_lockable = self.get_instance_lockable_ref(&iterable.obj.unwrap(), for_block.file_position.clone())?;
        let inst = inst_lockable.lock().unwrap();
        let mut inst_iterable = inst.into_iter();

        drop(inst); // FIXME can we somehow prevent dataraces here even with dropping?

        let for_result: Result<(_, _), RuntimeError> = inst_iterable.try_fold((Value::void(), false), |carry, (idx, val)| {
            if carry.1 {
                // there was a bail, so stop processing the loop
                return Ok(carry);
            }

            let mut inject = HashMap::new();

            inject.insert(k_name.identifier.clone(), (idx, k_name.file_position.clone()));
            inject.insert(v_name.identifier.clone(), (val, v_name.file_position.clone()));

            return self.visit_block(for_block, inject);
        });

        return for_result;
    }

    /// Visit an expression.
    fn visit_expression(&mut self, expression: &Expression) -> AstResult {
        let expr_position = expression.file_position.clone();

        match &expression.kind {
            ExpressionKind::LiteralValue(val) => Ok(val.clone()),
            ExpressionKind::BinaryOperation(lhs, op, rhs) => self.visit_binop_expression(lhs, op, rhs),
            ExpressionKind::UnaryOperation(unop, expr) => self.visit_unop(unop, expr),
            ExpressionKind::Assignment(identpath, expr) => self.visit_assignment(identpath, expr),
            ExpressionKind::FunctionCall(identpath, call_args) => self.visit_callable_call(identpath, call_args),
            ExpressionKind::IdentPath(..) => self.resolve_ident_path_to_value(&expression),
            ExpressionKind::NewInstance(ident, args, typeparams) => self.visit_instance_newup(ident, args, typeparams),
            ExpressionKind::ObjectAccess(lhs, accesskind, rhs) => self.visit_generic_object_access(lhs, accesskind, rhs),
            ExpressionKind::ListDeclaration(item_exprs) => self.visit_list_shorthand_expression(item_exprs, expr_position),
            ExpressionKind::DictDeclaration(item_exprs) => self.visit_dict_shorthand_expression(item_exprs, expr_position),
            _ => unimplemented!("{:?}", expression.kind)
        }
    }

    /// Visit a name assignment node.
    fn visit_assignment(&mut self, ident_path: &Expression, value_expr: &Expression) -> AstResult {
        let (owner_inst, _, access_kind, property) = self.resolve_ident_path(ident_path)?;
        let value = self.visit_expression(value_expr)?;

        if owner_inst.is_none() {
            // local ref assign
            return self.set_local_ref(property.identifier, value, &property.file_position, false);
        } else {
            // property assign
            let obj = owner_inst.unwrap();

            let inst_lockable = self.get_instance_lockable_ref(&obj.obj.unwrap(), property.file_position.clone())?;

            let is_static_access = match access_kind.unwrap() {
                AccessKind::Static => true,
                _ => false
            };

            let access = AccessParams {
                is_static_access: is_static_access,
                member_name: &property.identifier,
                accessor_instref: &self.self_ref,
                access_file_pos: &Some(property.file_position)
            };

            return inst_lockable.lock().unwrap().mutate_property(access, value);
        }
    }

    /// Visit and resolve an identifier path expression to a value.
    fn resolve_ident_path_to_value(&mut self, ident_path: &Expression) -> AstResult {
        let (root_inst, _, acckind, member) = self.resolve_ident_path(ident_path)?;

        if root_inst.is_none() {
            return self.get_local_ref(&member.identifier, &ident_path.file_position);
        }

        let access_val: Value = self.access_object_property(&root_inst.unwrap(), &acckind.unwrap_or(AccessKind::Instance), member)?;

        return Ok(access_val);
    }

    /// Visit a binary operation expression.
    fn visit_binop_expression(&mut self, lhs_expr: &Expression, binop: &BinOp, rhs_expr: &Expression) -> AstResult {
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
            BinOpKind::Or => self.visit_binop_or(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Eq => self.visit_binop_eq(lhs_expr, rhs_expr, &binop.file_position),
            BinOpKind::Neq => self.visit_binop_neq(lhs_expr, rhs_expr, &binop.file_position),
        }
    }

    /// Visit binop expression.
    fn visit_binop_add(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() + rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() + rhs_value.int.unwrap()),
            (SahaType::Str, SahaType::Str) => Value::str(format!("{}{}", lhs_value.str.unwrap(), rhs_value.str.unwrap())),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} + {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_sub(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() - rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() - rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} - {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_mul(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Float, SahaType::Float) => Value::float(lhs_value.float.unwrap() * rhs_value.float.unwrap()),
            (SahaType::Int, SahaType::Int) => Value::int(lhs_value.int.unwrap() * rhs_value.int.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} * {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_div(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value: Value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Float, SahaType::Float) => {
                if rhs_value.float.unwrap() == r64(0.0) {
                    let err = RuntimeError::new("Division by zero", Some(op_pos.clone()));

                    return Err(err);
                }

                Value::float(lhs_value.float.unwrap() / rhs_value.float.unwrap())
            },
            (SahaType::Int, SahaType::Int) => {
                if rhs_value.int.unwrap() == 0 {
                    let err = RuntimeError::new("Division by zero", Some(op_pos.clone()));

                    return Err(err);
                }

                Value::int(lhs_value.int.unwrap() / rhs_value.int.unwrap())
            },
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} / {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_gt(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() > rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() > rhs_value.float.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} > {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_gte(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() >= rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() >= rhs_value.float.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} >= {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_lt(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() < rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() < rhs_value.float.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} < {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_lte(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() <= rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() <= rhs_value.float.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} <= {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_eq(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() == rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() == rhs_value.float.unwrap()),
            (SahaType::Bool, SahaType::Bool) => Value::bool(lhs_value.bool.unwrap() == rhs_value.bool.unwrap()),
            (SahaType::Str, SahaType::Str) => Value::bool(lhs_value.str.unwrap() == rhs_value.str.unwrap()),
            (SahaType::Obj, SahaType::Obj) => Value::bool(lhs_value.obj.unwrap() == rhs_value.obj.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} == {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_neq(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;
        let rhs_value: Value = self.visit_expression(rhs)?;

        let (lk, rk) = (lhs_value.kind, rhs_value.kind);
        let (lkstr, rkstr) = (format!("{:?}", lk), format!("{:?}", rk));

        let new_val: Value = match (*lk, *rk) {
            (SahaType::Int, SahaType::Int) => Value::bool(lhs_value.int.unwrap() != rhs_value.int.unwrap()),
            (SahaType::Float, SahaType::Float) => Value::bool(lhs_value.float.unwrap() != rhs_value.float.unwrap()),
            (SahaType::Bool, SahaType::Bool) => Value::bool(lhs_value.bool.unwrap() != rhs_value.bool.unwrap()),
            (SahaType::Str, SahaType::Str) => Value::bool(lhs_value.str.unwrap() != rhs_value.str.unwrap()),
            (SahaType::Obj, SahaType::Obj) => Value::bool(lhs_value.obj.unwrap() != rhs_value.obj.unwrap()),
            _ => {
                let err = RuntimeError::new(
                    &format!("Mismatching operands for operation: `{:?} != {:?}`", lkstr, rkstr),
                    Some(op_pos.clone())
                );

                return Err(err);
            }
        };

        return Ok(new_val);
    }

    /// Visit binop expression.
    fn visit_binop_and(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        match *lhs_value.kind {
            SahaType::Bool => {
                if !lhs_value.bool.unwrap() {
                    // lhs is false so no point attempting to parse the right side for no reason
                    return Ok(Value::bool(false));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid left operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err);
            }
        };

        // we got this far because lhs is a true boolean value, lets check the right side

        let rhs_value: Value = self.visit_expression(rhs)?;

        match *rhs_value.kind {
            SahaType::Bool => {
                if !rhs_value.bool.unwrap() {
                    // rhs is false so no matter what lhs was, we return false
                    return Ok(Value::bool(false));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid right operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err);
            }
        };

        // both true!
        return Ok(Value::bool(true));
    }

    /// Visit binop expression.
    fn visit_binop_or(&mut self, lhs: &Expression, rhs: &Expression, op_pos: &FilePosition) -> AstResult {
        let lhs_value = self.visit_expression(lhs)?;

        match *lhs_value.kind {
            SahaType::Bool => {
                if lhs_value.bool.unwrap() {
                    // lhs is true so no point attempting to parse the right side for no reason
                    return Ok(Value::bool(true));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid left operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err);
            }
        };

        // we got this far because lhs is a true boolean value, lets check the right side

        let rhs_value: Value = self.visit_expression(rhs)?;

        match *rhs_value.kind {
            SahaType::Bool => {
                if rhs_value.bool.unwrap() {
                    // rhs is false so no matter what lhs was, we return false
                    return Ok(Value::bool(true));
                }
            },
            _ => {
                let err = RuntimeError::new("Invalid right operand for `&&`, not a boolean", Some(op_pos.clone()));

                return Err(err);
            }
        };

        // both false!
        return Ok(Value::bool(false));
    }

    fn visit_unop(&mut self, unop: &UnaryOp, expr: &Expression) -> AstResult {
        let expr_value = self.visit_expression(expr)?;

        let new_val: Value = match unop.kind {
            UnaryOpKind::Not => {
                if let SahaType::Bool = *expr_value.kind {
                    Value::bool(!expr_value.bool.unwrap())
                } else {
                    let err = RuntimeError::new("Invalid unary negation operand, expected boolean", Some(unop.file_position.clone()));

                    return Err(err);
                }
            },
            UnaryOpKind::Minus => {
                match *expr_value.kind {
                    SahaType::Int => Value::int(-expr_value.int.unwrap()),
                    SahaType::Float => Value::float(-expr_value.float.unwrap()),
                    _ => {
                        let err = RuntimeError::new("Invalid unary minus operand, expected int or float", Some(unop.file_position.clone()));

                        return Err(err);
                    }
                }
            }
        };

        return Ok(new_val);
    }

    /// Visit a list declaration shorthand expression (`[]` delimited items).
    fn visit_list_shorthand_expression(&mut self, items_exprs: &[Box<Expression>], pos: FilePosition) -> AstResult {
        // the first list item determines the wanted list parameter type
        let list_type: Box<SahaType>;
        let mut items: Vec<Value> = Vec::new();

        if items_exprs.is_empty() {
            let err = RuntimeError::new(
                "List shorthand expressions cannot be used for empty list declarations",
                Some(pos)
            );

            return Err(err);
        }

        for item in items_exprs {
            items.push(self.visit_expression(item)?);
        }

        list_type = items.first().unwrap().kind.clone();

        for i in &items {
            if i.kind != list_type {
                let err = RuntimeError::new(
                    &format!(
                        "List expects values of type {}, but received {} instead",
                        list_type.to_readable_string(),
                        i.kind.to_readable_string()
                    ),
                    Some(pos)
                );

                return Err(err);
            }
        }

        let mut item_args: SahaFunctionArguments = HashMap::new();
        let mut idx = 0;

        // this is a hack to make the items fit into a hashmap
        items.into_iter().for_each(|i| {
            item_args.insert("item_".to_string() + idx.to_string().as_str(), i.clone());

            idx += 1;
        });

        let list_instref: InstRef;
        let list_typeparams = vec![list_type.clone()];
        let create_args = HashMap::new();
        let new_inst_method_ref: CoreConstructorFn;

        {
            let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            list_instref = st.create_instref();
            new_inst_method_ref = st.core_classes["List"];
        };

        let list_instance = self.create_new_core_instance(
            list_instref,
            new_inst_method_ref,
            &create_args,
            &list_typeparams,
            &item_args,
            Some(pos)
        )?;

        {
            let mut st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();
            st.instances.insert(list_instref, Arc::new(Mutex::new(list_instance)));
        }

        let inst_val = Value::obj(list_instref);

        return Ok(inst_val);
    }

    /// Visit a dictionary shorthand declaration (`{...}` delimited).
    fn visit_dict_shorthand_expression(&mut self, items_exprs: &[(Box<Expression>, Box<Expression>)], pos: FilePosition) -> AstResult {
        let mut value_hmap: HashMap<String, Value> = HashMap::new();
        let mut dict_type: Option<Box<SahaType>> = None;

        if items_exprs.is_empty() {
            let err = RuntimeError::new(
                "Dict shorthand expressions cannot be used for empty dict declarations",
                Some(pos)
            );

            return Err(err);
        }

        for (k_expr, i_expr) in items_exprs {
            let k_pos = k_expr.file_position.clone();
            let k_val: Value = self.visit_expression(k_expr)?;
            let i_val: Value = self.visit_expression(i_expr)?;

            match *k_val.kind {
                SahaType::Str => {},
                _ => {
                    let err = RuntimeError::new(
                        &format!("Dict keys must be strings, `{}` received instead", k_val.kind.to_readable_string()),
                        Some(k_pos)
                    );

                    return Err(err);
                }
            };

            if dict_type.is_none() {
                dict_type = Some(i_val.kind.clone());
            }

            value_hmap.insert(k_val.str.unwrap(), i_val.clone());
        }

        let dict_type = dict_type.unwrap(); // be wary here, but we have items so we presumably have a type as well

        for i in value_hmap.values() {
            if i.kind != dict_type {
                let err = RuntimeError::new(
                    &format!(
                        "Dict expects values of type {}, but received {} instead",
                        dict_type.to_readable_string(),
                        i.kind.to_readable_string()
                    ),
                    Some(pos)
                );

                return Err(err);
            }
        }

        let dict_instref: InstRef;
        let dict_typeparams = vec![dict_type.clone()];
        let create_args = HashMap::new();
        let new_inst_method_ref: CoreConstructorFn;

        {
            let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            dict_instref = st.create_instref();
            new_inst_method_ref = st.core_classes["Dict"];
        };

        let dict_instance = self.create_new_core_instance(
            dict_instref,
            new_inst_method_ref,
            &create_args,
            &dict_typeparams,
            &value_hmap,
            Some(pos)
        )?;

        {
            let mut st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();
            st.instances.insert(dict_instref, Arc::new(Mutex::new(dict_instance)));
        }

        let inst_val = Value::obj(dict_instref);

        return Ok(inst_val);
    }

    /// Visit a callable call.
    fn visit_callable_call(&mut self, ident_path: &Expression, args: &Expression) -> AstResult {
        let (owner_inst, owner_class_name, acckind, callable) = self.resolve_ident_path(ident_path)?;

        if owner_inst.is_none() && owner_class_name.is_none() {
            return self.call_function(&callable, args);
        } else if owner_class_name.is_some() && acckind == Some(AccessKind::Static) {
            return self.call_static_method_with_classname(&owner_class_name.unwrap(), &AccessKind::Static, &callable, args);
        } else {
            let args = self.parse_callable_args(args)?;
            return self.call_method(&owner_inst.unwrap(), &acckind.unwrap_or(AccessKind::Instance), &callable, args);
        }
    }

    /// Resolve an identifier path. First tuple member is a instref value
    /// (optional), third is the method or property name.
    fn resolve_ident_path(&mut self, path: &Expression) -> Result<ResolvedIdentPath, RuntimeError> {
        let member: Identifier; // callable name
        let mut owner: Option<Value> = None; // instref
        let mut last_access_kind: Option<AccessKind> = None; // inst or static access?

        match &path.kind {
            ExpressionKind::IdentPath(root, members) => {
                if members.is_empty() {
                    member = root.clone();
                } else {
                    let maybe_a_static_class_call: bool = members.len() == 1 && members.first().unwrap().0 == AccessKind::Static;

                    let mut memberpath = members.clone();
                    let resolved_local_value = self.resolve_local_name(&root);

                    if resolved_local_value.is_err() && maybe_a_static_class_call {
                        return Ok((
                            None,
                            Some(SahaType::Name(root.identifier.clone(), root.type_params.clone())),
                            Some(AccessKind::Static),
                            members.first().unwrap().1.clone()
                        ));
                    }

                    let mut obj_being_accessed: Option<Value> = resolved_local_value.ok();

                    loop {
                        let (acckind, mname) = memberpath.remove(0);

                        if memberpath.is_empty() {
                            member = mname;
                            owner = obj_being_accessed;
                            last_access_kind = Some(acckind);
                            break;
                        }

                        obj_being_accessed = Some(self.access_object_property(&obj_being_accessed.unwrap(), &acckind, mname)?);
                    }
                }
            },
            _ => unreachable!()
        }

        return Ok((owner, None, last_access_kind, member));
    }

    /// Visit a generic object access expression.
    fn visit_generic_object_access(&mut self, lhs_expr: &Expression, access_kind: &AccessKind, rhs_expr: &Expression) -> AstResult {
        let lhs_value = self.visit_expression(lhs_expr)?;

        match &rhs_expr.kind {
            ExpressionKind::FunctionCall(identpath, call_args) => {
                let (_, _, _, ident) = self.resolve_ident_path(&identpath)?;
                let args = self.parse_callable_args(call_args)?;
                self.call_method(&lhs_value, &access_kind, &ident, args)
            },
            ExpressionKind::IdentPath(..) => {
                let (_, _, _, ident) = self.resolve_ident_path(rhs_expr)?;
                self.access_object_property(&lhs_value, access_kind, ident)
            },
            _ => unimplemented!()
        }
    }

    /// Access an object property and get the value it contains.
    fn access_object_property(&mut self, obj: &Value, access_kind: &AccessKind, property_name: Identifier) -> AstResult {
        match *obj.kind {
            SahaType::Obj => (),
            _ => {
                let err = RuntimeError::new(
                    &format!("Attempted to access property `{}` of a non-object value", property_name.identifier),
                    Some(property_name.file_position)
                );

                return Err(err);
            }
        };

        let inst_lockable = self.get_instance_lockable_ref(&obj.obj.unwrap(), property_name.file_position.clone())?;

        let is_static_access = match access_kind {
            AccessKind::Static => true,
            _ => false
        };

        let access = AccessParams {
            is_static_access: is_static_access,
            member_name: &property_name.identifier,
            accessor_instref: &self.self_ref,
            access_file_pos: &Some(property_name.file_position)
        };

        return inst_lockable.lock().unwrap().access_property(access);
    }

    /// Resolve an identifier name to a local ref table value.
    fn resolve_local_name(&mut self, name: &Identifier) -> AstResult {
        let refvalue: Value = self.get_local_ref(&name.identifier, &name.file_position)?;

        return Ok(refvalue);
    }

    fn parse_callable_args(&mut self, args: &Expression) -> Result<SahaFunctionArguments, RuntimeError> {
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
    fn call_function(&mut self, callable: &Identifier, args: &Expression) -> AstResult {
        let func: Box<dyn SahaCallable>;

        {
            let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            if !st.functions.contains_key(&callable.identifier) {
                let err = RuntimeError::new(
                    &format!("Cannot call undefined function `{}`", callable.identifier),
                    Some(callable.file_position.clone())
                );

                return Err(err);
            }

            let funcopt = &st.functions[&callable.identifier];

            func = funcopt.clone();
        }

        let call_args: SahaFunctionArguments = self.parse_callable_args(args)?;

        return func.call(call_args, None, Vec::new(), Some(callable.file_position.clone()));
    }

    /// Call an object method.
    fn call_method(&mut self, obj: &Value, access_kind: &AccessKind, callable: &Identifier, call_args: SahaFunctionArguments) -> AstResult {
        match *obj.kind {
            SahaType::Void => {
                let err = RuntimeError::new("Cannot access property or call method on a void value", Some(callable.file_position.clone()));

                return Err(err);
            },
            SahaType::Obj => {
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

                // intopt will be a cloned arc, meaning we have a cloned reference from an existing
                // arc
                let instopt: Arc<Mutex<Box<dyn SahaObject>>>;
                let method_ref: Arc<Box<dyn SahaCallable>>;
                let instref;
                let fqname;
                let inst_tparams;

                {
                    instopt = self.get_instance_lockable_ref(&obj.obj.unwrap(), callable.file_position.clone())?;

                    let mut instance = instopt.lock().unwrap();

                    if instance.is_core_defined() {
                        // we want to allow "pure" rust instances to be able to bypass the symbol
                        // table, and we should make sure no core method locks the instance inside
                        // these member calls
                        return instance.call_member(access, call_args);
                    }

                    // userland instance locks need to be dropped and methods called separately, to
                    // prevent lockups for instances

                    instref = instance.get_instance_ref();
                    fqname = instance.get_fully_qualified_class_name();
                    method_ref = instance.get_method_ref(&callable.identifier.as_str())?;
                    inst_tparams = instance.get_type_params();
                }

                self.call_instance_member(Some(instref),  &method_ref, access, &call_args, inst_tparams, &fqname)
            },
            _ => {
                obj.call_value_method(&callable.file_position, access_kind, &callable.identifier, &call_args)
            }
        }
    }

    /// Call a static method with just the class name, no instance.
    fn call_static_method_with_classname(
        &mut self,
        _class: &SahaType,
        _access_kind: &AccessKind,
        _callable: &Identifier,
        _args: &Expression
    ) -> AstResult {
        unimplemented!("Static access is not ready");
        /*
        let call_args: SahaFunctionArguments = self.parse_callable_args(args)?;

        let memberpos = callable.file_position.clone();
        let membername = callable.identifier.clone();

        if let SahaType::Name(ref n, ref ty) = *class {
            let static_method_name = format!("{}#{}", n, membername);
            let method_ref: Arc<Box<dyn SahaCallable>>;
            let call_ty: Vec<(char, Box<SahaType>)>;

            let access = AccessParams {
                is_static_access: true,
                member_name: &membername,
                accessor_instref: &self.self_ref.clone(),
                access_file_pos: &Some(memberpos.clone())
            };

            {
                let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

                if !st.classes.contains_key(n) && !st.core_classes.contains_key(n) {
                    let err = RuntimeError::new(&format!("Cannot call method of unknown class `{}`", n), Some(memberpos.clone()));

                    return Err(err);
                }

                if !st.methods.contains_key(&static_method_name) {
                    let err = RuntimeError::new(
                        &format!("No method `{}` defined in class `{}`", membername, n),
                        Some(memberpos)
                    );

                    return Err(err);
                }

                let class_ty = self.get_class_type_params(n);

                call_ty = class_ty.into_iter().zip(ty.clone().into_iter()).collect::<Vec<(char, Box<SahaType>)>>();
                method_ref = Arc::clone(st.methods.get(&static_method_name).unwrap());
            }

            return self.call_instance_member(
                None,
                method_ref,
                access,
                call_args,
                call_ty,
                n.to_string()
            );
        } else {
            let err = RuntimeError::new(
                &format!("Cannot call static method `{}` of value `{}`", membername, class.to_readable_string()),
                Some(memberpos)
            );

            return Err(err);
        }
        */
    }

    /// Call an instance method.
    fn call_instance_member(
        &self,
        instref: Option<InstRef>,
        method_ref: &Arc<Box<dyn SahaCallable>>,
        access: AccessParams,
        args: &SahaFunctionArguments,
        type_params: Vec<(char, Box<SahaType>)>,
        classname: &str
    ) -> AstResult {
        let member = access.member_name;
        let access_pos = access.access_file_pos;
        let static_access = access.is_static_access;
        let accessor_instref = access.accessor_instref;

        let is_self_internal_call = if instref.is_some() {
            match accessor_instref {
                Some(iref) => instref.unwrap() == *iref,
                _ => false
            }
        } else {
            false
        };

        let member_is_static = method_ref.is_static();
        let member_is_public = method_ref.is_public();
        let typeparammap: HashMap<_, _> = type_params.into_iter().collect();
        let member_ret_type = method_ref.get_return_type();

        let actual_return_type: Box<SahaType> = match *member_ret_type {
            SahaType::TypeParam(ty) => {
                let maybe_ty = typeparammap.get(&ty).unwrap_or(&Box::new(SahaType::Void)).clone();

                if maybe_ty == Box::new(SahaType::Void) {
                    let err = RuntimeError::new(
                        &format!("Method `{}` on class `{}` expects a type parameter `{}`, but none was defined", member, classname, ty),
                        access_pos.to_owned()
                    );

                    return Err(err);
                } else {
                    maybe_ty.clone()
                }
            },
            _ => member_ret_type
        };

        if !member_is_public && !is_self_internal_call {
            let err = RuntimeError::new(
                &format!("Attempted to call private method `{}` on class `{}`", member, classname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        if member_is_static && !static_access {
            let err = RuntimeError::new(
                &format!("Attempted to call static method `{}` unstatically on class `{}`", member, classname),
                access_pos.to_owned()
            );

            return Err(err);
        }

        // clone here to prevent any accidental side effects
        let mut call_args: SahaFunctionArguments = args.clone();

        if !member_is_static && instref.is_some() {
            // insert `self` to the call
            call_args.insert("self".to_string(), Value::obj(instref.unwrap()));
        }

        return method_ref.call(call_args, Some(actual_return_type), Vec::new(), access_pos.clone());
    }

    /// Visit a newup expression.
    fn visit_instance_newup(&mut self, ident: &Identifier, args: &Expression, typeparams: &[Box<SahaType>]) -> AstResult {
        let newup_args: SahaFunctionArguments = self.parse_callable_args(args)?;
        let inst_val: Value;
        let new_instref: InstRef;
        let mut user_inst_def: Option<ClassDefinition> = None;
        let mut core_inst_def: Option<CoreConstructorFn> = None;
        let created_inst: Box<dyn SahaObject>;

        {
            let st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();

            new_instref = st.create_instref();

            if st.classes.contains_key(&ident.identifier) {
                user_inst_def = st.classes.get(&ident.identifier).cloned();
            }

            if st.core_classes.contains_key(&ident.identifier) {
                core_inst_def = Some(st.core_classes[&ident.identifier]);
            }
        }

        // we run instance creation outside the symboltable lockup lifetime to
        // prevent race conditions when locking
        created_inst = match (user_inst_def, core_inst_def) {
            (Some(def), None) => self.create_new_instance(new_instref, &def, newup_args, typeparams, &Some(ident.file_position.clone()))?,
            (None, Some(fnref)) => self.create_new_core_instance(new_instref, fnref, &newup_args, typeparams, &HashMap::new(), Some(ident.file_position.clone()))?,
            _ => {
                let err = RuntimeError::new(
                    &format!("Cannot instantiate unknown class `{}`", ident.identifier),
                    Some(ident.file_position.clone())
                );

                return Err(err);
            }
        };

        {
            let mut st = crate::SAHA_SYMBOL_TABLE.lock().unwrap();
            st.instances.insert(new_instref, Arc::new(Mutex::new(created_inst)));
        }

        inst_val = Value::obj(new_instref);

        return Ok(inst_val);
    }

    /// Create a new core class instance.
    fn create_new_core_instance(
        &mut self,
        instref: InstRef,
        factory_fn: CoreConstructorFn,
        args: &SahaFunctionArguments,
        typeparams: &[Box<SahaType>],
        additional_args: &SahaFunctionArguments,
        create_pos: Option<FilePosition>
    ) -> Result<Box<dyn SahaObject>, RuntimeError> {
        return factory_fn(instref, args, typeparams, additional_args, create_pos);
    }

    /// Create a new instance from a userland class definition.
    fn create_new_instance(&mut self, instref: InstRef, def: &ClassDefinition, args: SahaFunctionArguments, typeparams: &[Box<SahaType>], create_pos: &Option<FilePosition>) -> Result<Box<dyn SahaObject>, RuntimeError> {
        return def.create_new_instance(instref, args, typeparams, &create_pos);
    }
}
