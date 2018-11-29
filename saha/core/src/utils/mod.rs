//! stdlib utils

use std::{
    sync::{Arc, Mutex},
    collections::HashMap
};

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
        },
        objects::{
            SahaObject
        }
    }
};

/// Get a new InstRef to be used on a new instance.
pub fn get_new_instref() -> InstRef {
    let st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

    return st.create_instref();
}

/// Insert an created saha object instance to the global symbol table and get
/// the instref value object for it.
pub fn add_instance_to_symbol_table(instref: InstRef, inst: Box<dyn SahaObject>) -> Value {
    let mut st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

    st.instances.insert(instref, Arc::new(Mutex::new(inst)));

    return Value::obj(instref);
}

/// Create a new core function to be inserted into the global symbol table as a
/// SahaCallable.
pub fn create_core_function(
    name: &str,
    params: Vec<(&str, FunctionParameter)>,
    return_type: Box<SahaType>,
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