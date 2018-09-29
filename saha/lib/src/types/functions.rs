//! Saha functions and related types

use std::collections::HashMap;
use std::any::Any;

use crate::{
    ast::Ast,
    types::{Value, SahaType},
    errors::{Error, RuntimeError},
    source::files::FilePosition,
    interpreter::AstVisitor
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

    /// Validate args in case there is only a single parameter defined.
    fn validate_single_param_args(&self, args: SahaFunctionArguments) -> Result<(), RuntimeError>;
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

    /// Is this a static member callable?
    fn is_static(&self) -> bool;

    /// Is this a public member callable?
    fn is_public(&self) -> bool;

    // internal
    fn as_any(&self) -> &dyn Any;

    // internal
    fn as_userfunction(&self) -> &UserFunction {
        let uf: &UserFunction = match self.as_any().downcast_ref::<UserFunction>() {
            Some(u) => u,
            None => panic!("Invalid internal callable cast, callable is not a user function")
        };

        return uf;
    }

    // internal
    fn as_corefunction(&self) -> &CoreFunction {
        let cf: &CoreFunction = match self.as_any().downcast_ref::<CoreFunction>() {
            Some(u) => u,
            None => panic!("Invalid internal callable cast, callable is not a core function")
        };

        return cf;
    }
}

/// Functions defined by the Saha core are CoreFunctions.
#[derive(Clone)]
pub struct CoreFunction {
    pub name: String,
    pub params: SahaFunctionParamDefs,
    pub return_type: SahaType,
    pub fn_ref: fn(args: SahaFunctionArguments) -> SahaCallResult,
    pub is_public: bool,
    pub is_static: bool
}

/// Functions defined by Saha developers in userland source code.
#[derive(Clone)]
pub struct UserFunction {
    pub source_name: String,
    pub name: String,
    pub params: SahaFunctionParamDefs,
    pub return_type: SahaType,
    pub ast: Ast,
    pub is_public: bool,
    pub is_static: bool
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

    fn is_public(&self) -> bool {
        return self.is_public;
    }

    fn is_static(&self) -> bool {
        return self.is_static;
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl SahaCallable for UserFunction {
    fn call(&self, args: SahaFunctionArguments, call_source_position: Option<FilePosition>) -> SahaCallResult {
        self.params.validate_args(args)?;

        let ast_visitor = AstVisitor::new(&self.ast);

        return ast_visitor.start();
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

    fn is_public(&self) -> bool {
        return self.is_public;
    }

    fn is_static(&self) -> bool {
        return self.is_static;
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ValidatesArgs for SahaFunctionParamDefs {
    /// Validate args in a situation where there is only a single parameter defined, which means
    /// we can call the function with no parameter name defined (to make code a little leaner).
    fn validate_single_param_args(&self, args: SahaFunctionArguments) -> Result<(), RuntimeError> {
        let param_name = self.keys().nth(0).unwrap();
        let param = self.values().nth(0).unwrap();
        let param_default = &param.default;
        let param_type = &param.param_type;

        if let SahaType::Void = param_default.kind {
            if args.is_empty() {
                // no arg and no default, which is a no-no
                let err = RuntimeError::new(
                    &format!("Invalid arguments, argument `{}` missing", param_name),
                    None
                );

                return Err(err.with_type("InvalidArgumentError"));
            } else {
                // no arg given, but a default is provided, so we're good
                return Ok(());
            }
        }

        let arg = args.values().nth(0).unwrap();

        // arg type mismatch
        if arg.kind != *param_type {
            let err = RuntimeError::new(
                &format!(
                    "Invalid argument, `{}` is expected to be a `{}`, found `{}` instead",
                    param_name,
                    param_type.to_readable_string(),
                    arg.kind.to_readable_string()
                ),
                None
            );

            return Err(err.with_type("InvalidArgumentError"));
        }

        // all OK for this arg

        return Ok(());
    }

    fn validate_args(&self, args: SahaFunctionArguments) -> Result<(), RuntimeError> {
        let mut allow_call_without_arg_name = false;

        if self.keys().len() == 1 {
            // if a function accepts only a single argument, we allow calling without setting a
            // parameter name (will use `""` internally)
            return self.validate_single_param_args(args);
        }

        for (name, ref param) in self {
            let param_type = param.param_type.to_owned();
            let param_default = param.default.to_owned();

            // arg missing, see if default is provided
            if args.contains_key(name) == false {
                match param_default.kind {
                    SahaType::Void => {
                        let err = RuntimeError::new(
                            &format!("Invalid arguments, argument `{}` missing", name),
                            None
                        );

                        return Err(err.with_type("InvalidArgumentError"));
                    }
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
