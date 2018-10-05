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
                name: "text".to_string(),
                param_type: SahaType::Str,
                default: Value::void()
            })
        ],
        SahaType::Void,
        print
    ));

    fns.push(create_core_function(
        "print_line",
        vec![
            ("text", FunctionParameter {
                name: "text".to_string(),
                param_type: SahaType::Str,
                default: Value::void()
            })
        ],
        SahaType::Void,
        print_line
    ));

    return fns;
}

fn print(args: SahaFunctionArguments) -> SahaCallResult {
    let to_print = args.get("text").unwrap().clone().str.unwrap();

    print!("{}", to_print);

    return Ok(Value::void());
}

fn print_line(args: SahaFunctionArguments) -> SahaCallResult {
    let to_print = args.get("text").unwrap().clone().str.unwrap();

    println!("{}", to_print);

    return Ok(Value::void());
}