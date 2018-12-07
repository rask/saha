//! Saha type system
//!
//! Saha has types defined as a single struct and unwrapping.

pub mod objects;
pub mod functions;
mod value_methods;

use noisy_float::prelude::*;

use std::{
    collections::HashMap,
    fmt::{
        Debug,
        Formatter as FmtFormatter,
        Result as FmtResult
    }
};

use crate::prelude::*;

use crate::{
    ast::AccessKind,
    types::value_methods::ValueMethodFn
};

lazy_static! {
    pub static ref str_methods: HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> = value_methods::get_str_methods();
    pub static ref int_methods: HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> = value_methods::get_int_methods();
    pub static ref float_methods: HashMap<String, (SahaFunctionParamDefs, ValueMethodFn)> = value_methods::get_float_methods();
}

/// UUID as bytes
pub type InstRef = [u8; 16];

/// Saha types.
#[derive(Clone, Debug, PartialEq)]
pub enum SahaType {
    /// Strings.
    Str,

    /// Integers.
    Int,

    /// Floats.
    Float,

    /// Booleans.
    Bool,

    /// Object instance references, instances are stored in the global symbol
    /// table.
    Obj,

    /// Names, which could be resolved to other types. Additionally stores
    /// type params/generic type data inside a vec. Type params are boxed in
    /// case they contain inner type params as well.
    Name(String, Vec<Box<SahaType>>),

    /// Type parameter, consisting of a single uppercase character.
    TypeParam(char),

    /// Internal void type.
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
            SahaType::Name(n, tp) => {
                if tp.len() == 0 {
                    n.to_owned()
                } else {
                    format!("{}<{:?}>", n.to_owned(), tp)
                }
            },
            SahaType::Obj => "object".to_string(),
            SahaType::TypeParam(c) => format!("Type param {}", c),
            _ => "void".to_string()
        }
    }
}

impl From<Value> for SahaType {
    fn from(value: Value) -> SahaType {
        return *value.kind;
    }
}

impl<'a> From<&'a Value> for SahaType {
    fn from(value: &'a Value) -> SahaType {
        return *value.kind.clone();
    }
}

impl<T> PartialEq<T> for SahaType where for<'a> SahaType: From<&'a T> {
    fn eq(&self, other: &T) -> bool {
        return self == &SahaType::from(other);
    }
}

/// A Saha value object.
#[derive(Clone)]
pub struct Value {
    pub kind: Box<SahaType>,
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
        let st = self.kind.clone();
        let ot = other.kind.clone();

        match (*st, *ot) {
            (SahaType::Bool, SahaType::Bool) => self.bool.unwrap() == other.bool.unwrap(),
            (SahaType::Str, SahaType::Str) => self.str.clone().unwrap() == other.str.clone().unwrap(),
            (SahaType::Int, SahaType::Int) => self.int.unwrap() == other.int.unwrap(),
            (SahaType::Float, SahaType::Float) => self.float.unwrap() == other.float.unwrap(),
            (SahaType::Obj, SahaType::Obj) => self.obj.unwrap() == other.obj.unwrap(),
            (SahaType::Void, SahaType::Void) => true,
            (SahaType::Name(ln, ltp), SahaType::Name(rn, rtp)) => {
                // classname and type params must match
                ln == rn && ltp == rtp
            },
            _ => false
        }
    }
}

impl Eq for Value {}

impl Default for Value {
    fn default() -> Value {
        return Value {
            kind: Box::new(SahaType::Void),
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

        let value_str = match *k {
            SahaType::Void => "void".to_string(),
            SahaType::Int => format!("{}", self.int.unwrap()),
            SahaType::Float => format!("{}", self.float.unwrap()),
            SahaType::Bool => format!("{}", self.bool.unwrap()),
            SahaType::Str => (self.str).clone().unwrap(),
            SahaType::Name(n, tp) => {
                if tp.len() == 0 {
                    n.to_owned()
                } else {
                    format!("{}<{:?}>", n.to_owned(), tp)
                }
            },
            SahaType::Obj => format!("{:?}", self.obj.unwrap()),
            SahaType::TypeParam(c) => format!("TypeParam {}", c)
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

        v.kind = Box::new(SahaType::Int);
        v.int = Some(int);

        return v;
    }

    /// Create a new float value.
    pub fn float(float: R64) -> Value {
        let mut v = Value::new();

        v.kind = Box::new(SahaType::Float);
        v.float = Some(float);

        return v;
    }

    /// Create a new str value.
    pub fn str(string: String) -> Value {
        let mut v = Value::new();

        v.kind = Box::new(SahaType::Str);
        v.str = Some(string);

        return v;
    }

    /// Create a new bool value.
    pub fn bool(boolean: bool) -> Value {
        let mut v = Value::new();

        v.kind = Box::new(SahaType::Bool);
        v.bool = Some(boolean);

        return v;
    }

    /// Create a new name value.
    pub fn name(name: String, type_params: Vec<Box<SahaType>>) -> Value {
        let mut v = Value::new();

        v.kind = Box::new(SahaType::Name(name.to_owned(), type_params));
        v.name = Some(name);

        return v;
    }

    /// Create a new obj ref value.
    pub fn obj(inst: InstRef) -> Value {
        let mut v = Value::new();

        v.kind = Box::new(SahaType::Obj);
        v.obj = Some(inst);

        return v;
    }

    /// Create a new void value.
    pub fn void() -> Value {
        return Value::new();
    }

    /// Internal. Call a value method, meaning our "raw" strings can be used
    /// like objects in userland code.
    ///
    /// ```saha
    /// var foo'str = "hello world";
    /// vat len'int = foo->length();
    /// ```
    pub fn call_value_method(&self, call_pos: &FilePosition, _: &AccessKind, method_name: &String, args: &SahaFunctionArguments) -> SahaCallResult {
        let valuemethods = match *self.kind {
            SahaType::Int => int_methods.clone(),
            SahaType::Str => str_methods.clone(),
            SahaType::Float => float_methods.clone(),
            _ => unimplemented!()
        };

        if valuemethods.contains_key(method_name) == false {
            let err = RuntimeError::new(
                &format!("No method `{}` defined for type `{:?}`", method_name, *self.kind),
                Some(call_pos.clone())
            );

            return Err(err);
        }

        let (mparams, mfn) = valuemethods.get(method_name).unwrap();

        mparams.validate_args(args, &Some(call_pos.clone()))?;

        return mfn(self.clone(), args.clone());
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
            (Value::name("Hello".to_string(), Vec::new()), Value::name("Hello".to_string(), Vec::new()), true),
            (Value::name("Hello".to_string(), Vec::new()), Value::name("Hello".to_string(), Vec::new(SahaType::Int)), false),
            (Value::name("Hello".to_string(), Vec::new()), Value::name("notHello".to_string(), Vec::new()), false),
        ];

        for (one, two, res) in test_vals {
            assert!((one == two) == res);
        }
    }
}
