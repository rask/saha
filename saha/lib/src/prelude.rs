//! Saha Library Prelude
//!
//! Includes the common bits and pieces which are used around the Saha codebase.

pub use crate::{
    source::files::FilePosition,
    errors::{
        Error,
        RuntimeError,
        ParseError
    },
    types::{
        Value,
        SahaType,
        InstRef,
        objects::{
            AccessParams,
            SahaObject,
            MemberVisibility
        },
        functions::{
            SahaCallable,
            SahaCallResult,
            SahaFunctionArguments,
            ValidatesArgs,
            SahaFunctionParamDefs,
            FunctionParameter,
            CoreFunction
        }
    }
};