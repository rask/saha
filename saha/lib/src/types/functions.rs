//! Saha functions and related types

use std::collections::HashMap;

use ::{
    types::{Value, SahaType},
    errors::{Error, RuntimeError},
    source::FilePosition,
};

/// A result type for Saha callable `call`s. Returns either a Saha Value object
/// or a RuntimeError.
pub type SahaCallResult = Result<Value, RuntimeError>;

/// Anything which can be called in Saha. Functions and methods mainly.
pub trait SahaCallable: Send {
    /// Call this callable.
    fn call(&self, args: Vec<()>, call_source_position: Option<FilePosition>) -> SahaCallResult;

    /// Get the parameters that this callable accepts.
    fn get_parameters(&self) -> Vec<()>;

    /// Get the return type this callable should return.
    fn get_return_type(&self) -> SahaType;
}

/// Functions defined by the Saha core are CoreFunctions.
pub struct CoreFunction {
    name: String,
    params: Vec<()>,
    return_type: SahaType,
    fn_ref: fn(args: Vec<()>) -> SahaCallResult
}

impl SahaCallable for CoreFunction {
    fn call(&self, args: Vec<()>, call_source_position: Option<FilePosition>) -> SahaCallResult {
        let err = RuntimeError::new("Not implemented", call_source_position);

        return Err(err.with_type("UnimplementedError".to_string()));
    }

    fn get_parameters(&self) -> Vec<()> {
        return self.params.to_owned();
    }

    fn get_return_type(&self) -> SahaType {
        return self.return_type.clone()
    }
}