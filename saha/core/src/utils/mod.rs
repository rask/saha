//! stdlib utils

use std::collections::HashMap;

use saha_lib::{
    errors::RuntimeError,
    symbol_table::InstRef,
    types::{
        Value, SahaType,
        functions::{
            SahaFunctionParamDefs,
            SahaFunctionArguments,
            FunctionParameter,
            SahaCallResult,
            CoreFunction
        }
    }
};

/// Create a new core function to be inserted into the global symbol table as a
/// SahaCallable.
pub fn create_core_function(
    name: &str,
    params: Vec<(&str, FunctionParameter)>,
    return_type: SahaType,
    rust_fn: fn(args: SahaFunctionArguments) -> SahaCallResult
) -> (String, CoreFunction) {
    let mut fn_params: SahaFunctionParamDefs = HashMap::new();

    for (pname, p) in params {
        fn_params.insert(pname.to_owned(), p);
    }

    let corefn = CoreFunction {
        name: name.to_owned(),
        params: fn_params,
        return_type: return_type,
        fn_ref: rust_fn,
        is_public: true,
        is_static: false
    };

    return (name.to_owned(), corefn);
}

/// Create a core method to use on an instance as a method.
pub fn create_core_method(
    name: &str,
    params: Vec<(&str, FunctionParameter)>,
    return_type: SahaType,
    rust_fn: fn(args: SahaFunctionArguments, param_types: Vec<(char, SahaType)>) -> SahaCallResult,
    is_public: bool,
    is_static: bool,
) -> (String, CoreFunction) {
    let mut fn_params: SahaFunctionParamDefs = HashMap::new();

    for (pname, p) in params {
        fn_params.insert(pname.to_owned(), p);
    }

    unimplemented!()
}
