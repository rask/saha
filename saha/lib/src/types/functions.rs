//! Saha functions and related types

use std::collections::HashMap;

use crate::{
    types::{Value, SahaType},
    errors::{Error, RuntimeError},
    source::FilePosition,
};

/// A result type for Saha callable `call`s. Returns either a Saha Value object
/// or a RuntimeError.
pub type SahaCallResult = Result<Value, RuntimeError>;

/// Collection of Saha function parameter definitions.
pub type SahaFunctionParamDefs = HashMap<String, FunctionParameter>;

/// A function call argument collection. Names mapped to Saha Values.
pub type SahaFunctionArguments = HashMap<String, Value>;

/// A single function parameter which Saha functions can accept.
#[derive(Clone)]
pub struct FunctionParameter {
    pub param_type: SahaType,
    pub default: Value
}

/// Anything that needs to validate call arguments.
pub trait ValidatesArgs {
    /// Validate a collection of function/method call arguments.
    fn validate_args(&self, args: SahaFunctionArguments) -> Result<(), RuntimeError>;
}

/// Anything which can be called in Saha. Functions and methods mainly.
pub trait SahaCallable: Send {
    /// Call this callable.
    fn call(&self, args: SahaFunctionArguments, call_source_position: Option<FilePosition>) -> SahaCallResult;

    /// Get the parameters that this callable accepts.
    fn get_parameters(&self) -> SahaFunctionParamDefs;

    /// Get the return type this callable should return.
    fn get_return_type(&self) -> SahaType;

    /// Get the name of the function. Not necessarily the name with which it appears in source code.
    fn get_name(&self) -> String;

    /// Get the name of this function as it appears in source code.
    fn get_source_name(&self) -> String;
}

/// Functions defined by the Saha core are CoreFunctions.
pub struct CoreFunction {
    name: String,
    params: SahaFunctionParamDefs,
    return_type: SahaType,
    fn_ref: fn(args: SahaFunctionArguments) -> SahaCallResult
}

/// Functions defined by Saha developers in userland source code.
pub struct UserFunction {
    source_name: String,
    name: String,
    params: SahaFunctionParamDefs,
    return_type: SahaType,
    ast: ()
}

impl SahaCallable for CoreFunction {
    fn call(&self, args: SahaFunctionArguments, call_source_position: Option<FilePosition>) -> SahaCallResult {
        self.params.validate_args(args)?;

        let err = RuntimeError::new("Not implemented", call_source_position);

        return Err(err.with_type("UnimplementedError"));
    }

    fn get_parameters(&self) -> SahaFunctionParamDefs {
        return self.params.clone();
    }

    fn get_return_type(&self) -> SahaType {
        return self.return_type.clone()
    }

    fn get_name(&self) -> String {
        return self.name.to_owned();
    }

    fn get_source_name(&self) -> String {
        return self.get_name();
    }
}

impl SahaCallable for UserFunction {
    fn call(&self, args: SahaFunctionArguments, call_source_position: Option<FilePosition>) -> SahaCallResult {
        self.params.validate_args(args)?;

        let err = RuntimeError::new("Not implemented", call_source_position);

        return Err(err.with_type("UnimplementedError"));
    }

    fn get_parameters(&self) -> SahaFunctionParamDefs {
        return self.params.clone();
    }

    fn get_return_type(&self) -> SahaType {
        return self.return_type.clone();
    }

    fn get_name(&self) -> String {
        return self.name.to_owned();
    }

    fn get_source_name(&self) -> String {
        return self.source_name.to_owned();
    }
}

impl ValidatesArgs for SahaFunctionParamDefs {
    fn validate_args(&self, args: SahaFunctionArguments) -> Result<(), RuntimeError> {
        for (name, ref param) in self {
            let param_type = param.param_type.to_owned();
            let param_default = param.default.to_owned();

            // arg missing, see if default is provided
            if args.contains_key(name) == false {
                match param_default.kind {
                    SahaType::Void => return Err(
                        RuntimeError::new(
                            &format!("Invalid arguments, argument `{}` missing", name),
                            None
                        )
                    ),
                    _ => ()
                };
            }

            let arg = args.get(name).unwrap();

            // arg type mismatch
            if arg.kind != param_type {
                let err = RuntimeError::new(
                    &format!(
                        "Invalid argument, `{}` is expected to be a `{}`, found `{}` instead",
                        name,
                        param_type.to_readable_string(),
                        arg.kind.to_readable_string()
                    ),
                    None
                );

                return Err(err.with_type("InvalidArgumentError"));
            }

            // all OK for this arg, continue loop
        }

        return Ok(());
    }
}
