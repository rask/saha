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
        functions::{SahaCallable, SahaCallResult, ValidatesArgs, FunctionParameter, SahaFunctionParamDefs, SahaFunctionArguments}
    }
};

/// Create a new Result instance.
pub fn new_instance(
    instref: InstRef,
    args: SahaFunctionArguments,
    type_params: &Vec<Box<SahaType>>,
    additional_data: SahaFunctionArguments,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 2 {
        let err = RuntimeError::new("`List` expects two type parameters: `T` and `U`", create_pos);

        return Err(err);
    }

    if args.len() > 0 {
        let err = RuntimeError::new("`Result` expects no arguments", create_pos);

        return Err(err);
    }

    // FIXME type param validation

    let initial_value: Value;
    let is_initially_success: bool;

    if additional_data.is_empty() {
        initial_value = Value::void();
        is_initially_success = false;
    } else {
        unimplemented!()
    }

    let result_inst = Box::new(SahaResult {
        success_type: type_params[0].clone(),
        fail_type: type_params[1].clone(),
        result_value: initial_value,
        is_success: is_initially_success,
        instref: instref
    });

    return Ok(result_inst);
}

/// SahaResult is the result type generic class for result outcomes in Saha.
#[derive(Clone)]
pub struct SahaResult {
    instref: InstRef,
    pub success_type: Box<SahaType>,
    pub fail_type: Box<SahaType>,
    pub result_value: Value,
    pub is_success: bool
}

impl SahaObject for SahaResult {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref.clone();
    }

    fn is_core_defined(&self) -> bool {
        return true;
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

    fn get_type_params(&self) -> Vec<(char, Box<SahaType>)> {
        return vec![
            ('T', self.success_type.clone()),
            ('U', self.fail_type.clone())
        ];
    }

    fn get_named_type(&self) -> Box<SahaType> {
        return Box::new(SahaType::Name(
            "Result".to_string(),
            vec![self.success_type.clone(), self.fail_type.clone()]
        ));
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            match access.member_name as &str {
                _ => Err(RuntimeError::new(
                    &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                    access.access_file_pos.clone()
                ))
            }
        } else {
            match access.member_name as &str {
                "succeed" => self.succeed(args, access),
                "fail" => self.fail(args, access),
                "isSuccess" => Ok(Value::bool(self.is_success)),
                "isFailed" => Ok(Value::bool(!self.is_success)),
                "unwrap" => Ok(self.result_value.clone()),
                _ => {
                    Err(RuntimeError::new(
                        &format!("No method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                        access.access_file_pos.clone()
                    ))
                }
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

    fn into_iter(&self) -> Box<Iterator<Item = (Value, Value)>> {
        unimplemented!()
    }

    fn set_data_from_iter(&mut self, iterator: Box<Iterator<Item = (Value, Value)>>) {
        unimplemented!()
    }
}

impl SahaResult {
    pub fn new_failure(instref: InstRef, initial_value: Value, success_type: Box<SahaType>, fail_type: Box<SahaType>) -> Box<dyn SahaObject> {
        return Box::new(SahaResult {
            is_success: false,
            result_value: initial_value,
            success_type: success_type,
            fail_type: fail_type,
            instref: instref
        });
    }

    pub fn new_success(instref: InstRef, initial_value: Value, success_type: Box<SahaType>, fail_type: Box<SahaType>) -> Box<dyn SahaObject> {
        return Box::new(SahaResult {
            is_success: true,
            result_value: initial_value,
            success_type: success_type,
            fail_type: fail_type,
            instref: instref
        });
    }

    /// This defines the parameters the succeed function requires.
    fn succeed_params(&self) -> SahaFunctionParamDefs {
        let mut params = HashMap::new();

        params.insert("value".to_string(), FunctionParameter {
            name: "value".to_string(),
            param_type: self.success_type.clone(),
            default: Value::void()
        });

        return params;
    }

    /// This makes the Result succeed with a given value.
    pub fn succeed(&mut self, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.succeed_params().validate_args(&args, access.access_file_pos)?;

        self.is_success = true;
        self.result_value = args.get("value").unwrap().clone();

        return Ok(Value::void());
    }

    /// This defines the parameters the fail function requires.
    fn fail_params(&self) -> SahaFunctionParamDefs {
        let mut params = HashMap::new();

        params.insert("value".to_string(), FunctionParameter {
            name: "value".to_string(),
            param_type: self.success_type.clone(),
            default: Value::void()
        });

        return params;
    }

    /// This makes the Result fail with a given value.
    pub fn fail(&mut self, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.fail_params().validate_args(&args, access.access_file_pos)?;

        self.is_success = false;
        self.result_value = args.get("value").unwrap().clone();

        return Ok(Value::void());
    }
}
