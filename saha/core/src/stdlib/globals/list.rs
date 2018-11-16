//! list.rs
//!
//! Defines the internal global `List<T>` type object.

use std::{
    collections::HashMap,
    sync::{Arc, Mutex}
};

use saha_lib::{
    symbol_table::InstRef,
    errors::{Error, RuntimeError},
    source::files::FilePosition,
    types::{
        Value, SahaType,
        objects::{ClassDefinition, AccessParams, Property, ObjProperties, SahaObject},
        functions::{SahaCallable, SahaCallResult, SahaFunctionArguments, ValidatesArgs, SahaFunctionParamDefs, FunctionParameter}
    }
};

/// Create a new List instance.
pub fn new_instance(
    instref: InstRef,
    args: SahaFunctionArguments,
    type_params: &Vec<SahaType>,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 1 {
        let err = RuntimeError::new("`List` expects a single type parameter `T`", create_pos);

        return Err(err);
    }

    if args.len() > 0 {
        let err = RuntimeError::new("`List` expects no arguments", create_pos);

        return Err(err);
    }

    let list_inst = Box::new(SahaList {
        param_type: type_params[0].clone(),
        data: vec![],
        instref: instref
    });

    return Ok(list_inst);
}

#[derive(Clone, Debug)]
struct SahaList {
    instref: InstRef,
    param_type: SahaType,
    data: Vec<Value>
}

impl SahaObject for SahaList {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref.clone();
    }

    fn get_class_name(&self) -> String {
        return "List".to_string();
    }

    fn get_fully_qualified_class_name(&self) -> String {
        return self.get_class_name();
    }

    fn get_implements(&self) -> Vec<String> {
        return vec![];
    }

    fn get_full_method_name(&mut self, method_name: &str) -> String {
        unimplemented!()
    }

    fn get_method_ref(&mut self, method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError> {
        unimplemented!()
    }

    fn get_type_params(&self) -> Vec<(char, SahaType)> {
        return vec![('T', self.param_type.clone())];
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            return Err(RuntimeError::new(
                &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                access.access_file_pos.clone()
            ));
        }

        let list_type = self.get_type_params()[0].1.clone();

        match access.member_name as &str {
            // FIXME struct state does not seem to pass trait boundary here
            //"push" => list_push(list_type, &mut self.data, args, access),
            //"count" => list_count(&self.data, args, access),
            "push" => self.push(access, args),
            "count" => self.count(access, args),
            _ => {
                return Err(RuntimeError::new(
                    &format!("No method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                    access.access_file_pos.clone()
                ));
            }
        }
    }

    fn access_property(&self, access: AccessParams) -> SahaCallResult {
        unimplemented!()
    }

    fn mutate_property(&mut self, access: AccessParams, new_value: Value) -> SahaCallResult {
        unimplemented!()
    }

    fn box_clone(&self) -> Box<dyn SahaObject> {
        return Box::new(self.clone());
    }
}

/// Get function parameter definition for the List::push method.
fn list_push_params(ty: &SahaType) -> SahaFunctionParamDefs {
    let mut params: SahaFunctionParamDefs = HashMap::new();

    params.insert("value".to_string(), FunctionParameter {
        name: "value".to_string(),
        param_type: ty.clone(),
        default: Value::void()
    });

    return params;
}

/// The List::push "method".
fn list_push(ty: SahaType, data: &mut Vec<Value>, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
    list_push_params(&ty).validate_args(&args, access.access_file_pos)?;

    let pushed_val = args.get("value").unwrap();

    data.push(pushed_val.clone());

    return Ok(Value::void());
}

/// The List::count "method".
fn list_count(data: &Vec<Value>, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
    let params: SahaFunctionParamDefs = HashMap::new(); // no params

    params.validate_args(&args, access.access_file_pos)?;

    let count = data.len();

    return Ok(Value::int(count as isize));
}

impl SahaList {
    /// Pushes a value to the end of a list object.
    fn push(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if args.contains_key("value") == false {
            let err = RuntimeError::new("Invalid arguments, `value` is required", access.access_file_pos.clone());

            return Err(err);
        }

        if args.len() > 1 {
            for (argname, _) in &args {
                if argname == "value" {
                    continue;
                }

                let err = RuntimeError::new(&format!("Unknown argument `{}` given", argname), access.access_file_pos.clone());

                return Err(err);
            }
        }

        let pushed_val = args.get("value").unwrap();

        if pushed_val.kind != self.param_type {
            let err = RuntimeError::new(&format!("Invalid argument `value`: expected type `{:?}`, received `{:?}`", self.param_type, pushed_val.kind), access.access_file_pos.clone());

            return Err(err);
        }

        self.data.push(pushed_val.clone());

        return Ok(Value::void());
    }

    /// Returns the count of items inside the list.
    fn count(&self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if args.len() > 0 {
            let err = RuntimeError::new("Invalid arguments, no arguments expected", access.access_file_pos.clone());

            return Err(err);
        }

        println!("is actually {:?}", self.data);

        let count: isize = self.data.len() as isize;

        return Ok(Value::int(count));
    }
}
