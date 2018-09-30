///! Saha stdlib globals

use std::collections::HashMap;

use saha_lib::types::{
    Value, SahaType,
    functions::{
        CoreFunction,
        SahaFunctionArguments,
        SahaCallResult,
        FunctionParameter,
    }
};

use crate::utils::create_core_function;

/// Register module stdlib parts.
pub fn get_saha_functions() -> Vec<(String, CoreFunction)> {
    let mut fns: Vec<(String, CoreFunction)> = Vec::new();

    fns.push(create_core_function(
        "print",
        vec![
            ("text", FunctionParameter {
                param_type: SahaType::Str,
                default: Value::void()
            })
        ],
        SahaType::Void,
        print
    ));

    return fns;
}

pub fn print(args: SahaFunctionArguments) -> SahaCallResult {
    return Ok(Value::void());
}