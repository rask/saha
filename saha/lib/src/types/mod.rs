//! Saha type system
//!
//! Saha has types defined as a single struct and unwrapping.

pub mod objects;
pub mod functions;

use noisy_float::prelude::*;

use ::symbol_table::InstRef;

/// Saha types.
#[derive(Clone)]
pub enum SahaType {
    /// Strings
    Str,

    /// Integers
    Int,

    /// Floats
    Float,

    /// Booleans
    Bool,

    /// Names, which should be resolved to other types
    Name(String),

    /// Object instance references, instances are stored in the global symbol
    /// table
    Obj,

    /// Internal void type
    Void,
}

/// A Saha value object.
pub struct Value {
    kind: SahaType,
    str: Option<String>,
    int: Option<isize>,
    float: Option<R64>,
    bool: Option<bool>,
    name: Option<String>,
    obj: Option<InstRef>,
    void: Option<bool>
}