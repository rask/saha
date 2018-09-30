//! Saha value methods
//!
//! Methods that are bound to values that are not object instances.

use std::collections::HashMap;
use crate::types::{
    Value, SahaType,
    functions::{SahaFunctionParamDefs, FunctionParameter, SahaFunctionArguments, SahaCallResult}
};

pub type ValueMethodFn = fn(caller: Value, args: SahaFunctionArguments) -> SahaCallResult;

/// Get value methods that are tied to `str` values.
pub fn get_str_methods() -> HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> {
    return HashMap::new();
}

/// Get value methods that are tied to `int` values.
pub fn get_int_methods() -> HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> {
    let mut fns: HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> = HashMap::new();

    fns.insert("to_string".to_string(), (HashMap::new(), int_to_string));

    return fns;
}

/// Convert `int` to `str`.
pub fn int_to_string(caller: Value, _: SahaFunctionArguments) -> SahaCallResult {
    let as_string = caller.int.unwrap().to_string();

    return Ok(Value::str(as_string));
}