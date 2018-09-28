//! Saha type system
//!
//! Saha has types defined as a single struct and unwrapping.

pub mod objects;
pub mod functions;

use noisy_float::prelude::*;

use std::{
    fmt::{
        Debug,
        Formatter as FmtFormatter,
        Result as FmtResult
    }
};

use crate::symbol_table::InstRef;

/// Saha types.
#[derive(Clone, Debug, PartialEq)]
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

impl SahaType {
    /// Get a readable string, as in display for error messages and such.
    pub fn to_readable_string(&self) -> String {
        return match self {
            SahaType::Bool => "bool".to_string(),
            SahaType::Str => "str".to_string(),
            SahaType::Int => "int".to_string(),
            SahaType::Float => "float".to_string(),
            SahaType::Name(n) => n.to_owned(),
            SahaType::Obj => "object".to_string(),
            _ => "void".to_string()
        }
    }
}

/// A Saha value object.
#[derive(Clone)]
pub struct Value {
    pub kind: SahaType,
    pub str: Option<String>,
    pub int: Option<isize>,
    pub float: Option<R64>,
    pub bool: Option<bool>,
    pub name: Option<String>,
    pub obj: Option<InstRef>,
    pub void: ()
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        return match self.kind {
            SahaType::Bool => self.bool == other.bool,
            SahaType::Str => self.str == other.str,
            SahaType::Int => self.int == other.int,
            SahaType::Float => self.float == other.float,
            SahaType::Name(ref n) => match other.kind {
                SahaType::Name(ref on) => n == on,
                _ => false
            },
            SahaType::Obj => self.obj == other.obj,
            _ => false
        }
    }
}

impl Eq for Value {}

impl Default for Value {
    fn default() -> Value {
        return Value {
            kind: SahaType::Void,
            str: None,
            bool: None,
            int: None,
            float: None,
            name: None,
            obj: None,
            void: ()
        };
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut FmtFormatter) -> FmtResult {
        let k = self.kind.clone();

        let value_str = match k {
            SahaType::Void => "void".to_string(),
            SahaType::Int => format!("{}", self.int.unwrap()),
            SahaType::Float => format!("{}", self.float.unwrap()),
            SahaType::Bool => format!("{}", self.bool.unwrap()),
            SahaType::Str => (self.str).clone().unwrap(),
            SahaType::Name(n) => n.to_owned(),
            SahaType::Obj => format!("{:?}", self.obj.unwrap())
        };

        return write!(f, "Value::{:?}({})", self.kind, value_str);
    }
}

impl Value {
    /// Get a new empty (void) value.
    pub fn new() -> Value {
        return Value::default();
    }

    /// Create a new int value.
    pub fn int(int: isize) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Int;
        v.int = Some(int);

        return v;
    }

    /// Create a new float value.
    pub fn float(float: R64) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Float;
        v.float = Some(float);

        return v;
    }

    /// Create a new str value.
    pub fn str(string: String) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Str;
        v.str = Some(string);

        return v;
    }

    /// Create a new bool value.
    pub fn bool(boolean: bool) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Bool;
        v.bool = Some(boolean);

        return v;
    }

    /// Create a new name value.
    pub fn name(name: String) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Name(name.to_owned());
        v.name = Some(name);

        return v;
    }

    /// Create a new obj ref value.
    pub fn obj(inst: InstRef) -> Value {
        let mut v = Value::new();

        v.kind = SahaType::Obj;
        v.obj = Some(inst);

        return v;
    }

    /// Create a new void value.
    pub fn void() -> Value {
        return Value::new();
    }
}

#[cfg(tests)]
mod tests {
    use super::*;

    #[test]
    fn test_value_comparisons_work() {
        let test_vals = vec![
            (Value::void(), Value::void(), false),
            (Value::void(), Value::bool(false), false),
            (Value::bool(true), Value::bool(false), false),
            (Value::bool(true), Value::bool(true), true),
            (Value::str("asdf".to_string()), Value::str("qwer".to_string()), false),
            (Value::str("asdf".to_string()), Value::str("asdf".to_string()), true),
            (Value::name("Hello".to_string()), Value::name("Hello".to_string()), true),
            (Value::name("Hello".to_string()), Value::name("notHello".to_string()), false),
        ];

        for (one, two, res) in test_vals {
            assert!((one == two) == res);
        }
    }
}
