//! result.rs
//!
//! Anything related to the `Result<T, U>` type used for error and result
//! management in Saha. Very similar to the `Result` type in Rust.

use std::{
    collections::HashMap,
    sync::{Arc, Mutex}
};

use saha_lib::{
    symbol_table::InstRef,
    errors::{Error, RuntimeError},
    source::files::FilePosition,
    types::{
        Value, SahaType,
        objects::{ClassDefinition, AccessParams, Property, ObjProperties, SahaObject},
        functions::{SahaCallable, SahaCallResult, SahaFunctionArguments}
    }
};

/// Create a new Result instance.
pub fn new_instance(
    instref: InstRef,
    args: SahaFunctionArguments,
    type_params: &Vec<SahaType>,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 2 {
        let err = RuntimeError::new("`List` expects two type parameters: `T` and `U`", create_pos);

        return Err(err.with_type("InvalidArgumentError"));
    }

    if args.len() > 0 {
        let err = RuntimeError::new("`Result` expects no arguments", create_pos);

        return Err(err.with_type("InvalidArgumentError"));
    }

    let list_inst = Box::new(SahaList {
        success_type: type_params[0].clone(),
        fail_type: type_params[0].clone(),
        value: Value::void(),
        is_success: false,
        instref: instref
    });

    return Ok(result_inst);
}

/// SahaResult is the result type generic class for result outcomes in Saha.
pub struct SahaResult {
    instref: InstRef,
    success_type: SahaType,
    fail_type: SahaType,
    result_value: Value,
    is_success: bool
}

impl SahaObject for SahaResult {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref.clone();
    }

    fn get_class_name(&self) -> String {
        return "Result".to_string();
    }

    fn get_fully_qualified_class_name(&self) -> String {
        return self.get_class_name();
    }

    fn get_implements(&self) -> Vec<String> {
        return vec![];
    }

    fn get_full_method_name(&mut self, method_name: &str) -> String {
        unimplemented!()
    }

    fn get_method_ref(&mut self, method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError> {
        unimplemented!()
    }

    fn get_type_params(&self) -> Vec<(char, SahaType)> {
        return vec![
            ('T', self.success_type.clone()),
            ('U', self.fail_type.clone())
        ];
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            return Err(RuntimeError::new(
                &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                access.access_file_pos.clone()
            ));
        }

        match access.member_name as &str {
            // FIXME struct state does not seem to pass trait boundary here
            _ => {
                return Err(RuntimeError::new(
                    &format!("No method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                    access.access_file_pos.clone()
                ));
            }
        }
    }

    fn access_property(&self, access: AccessParams) -> SahaCallResult {
        unimplemented!()
    }

    fn mutate_property(&mut self, access: AccessParams, new_value: Value) -> SahaCallResult {
        unimplemented!()
    }

    fn box_clone(&self) -> Box<dyn SahaObject> {
        return Box::new(self.clone());
    }
}

impl SahaResult {
    pub fn fail(&mut self, with_value: Value) -> SahaResult {
        unimplemented!()
    }

    pub fn succeed(&mut self, with_value: Value) -> SahaResult {
        unimplemented!()
    }
}