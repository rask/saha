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
        functions::{SahaCallable, SahaCallResult, SahaFunctionArguments}
    }
};

pub fn new_instance(
    instref: InstRef,
    args: SahaFunctionArguments,
    type_params: HashMap<char, SahaType>,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 1 {
        let err = RuntimeError::new("`List` expects a single type parameter `T`", create_pos);

        return Err(err.with_type("InvalidArgumentError"));
    }

    if type_params.keys().nth(0).unwrap() != &'T' {
        let err = RuntimeError::new("`List` expects a single type parameter `T`", create_pos);

        return Err(err.with_type("InvalidArgumentError"));
    }

    if args.len() > 0 {
        let err = RuntimeError::new("`List` expects no arguments", create_pos);

        return Err(err.with_type("InvalidArgumentError"));
    }

    let list_inst = Box::new(SahaList {
        param_type: type_params.get(&'T').unwrap().clone(),
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

    fn get_type_params(&self) -> HashMap<char, SahaType> {
        let mut type_params = HashMap::new();

        type_params.insert('T', self.param_type.clone());

        return type_params;
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            return Err(RuntimeError::new(
                &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                access.access_file_pos.clone()
            ));
        }

        match access.member_name as &str {
            "push" => SahaList::push(self, access, args),
            "count" => SahaList::count(self, access, args),
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

impl SahaList {
    /// Pushes a value to the end of a list object.
    pub fn push(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if args.contains_key("value") == false {
            let err = RuntimeError::new("Invalid arguments, `value` is required", access.access_file_pos.clone());

            return Err(err.with_type("InvalidArgumentError"));
        }

        let pushed_val = args.get("value").unwrap();

        if pushed_val.kind != self.param_type {
            let err = RuntimeError::new(&format!("Invalid argument, expected type `{:?}`, received `{:?}`", self.param_type, pushed_val.kind), access.access_file_pos.clone());

            return Err(err.with_type("InvalidArgumentError"));
        }

        self.data.push(pushed_val.clone());

        return Ok(Value::void());
    }

    /// Returns the count of items inside the list.
    pub fn count(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if args.len() > 0 {
            let err = RuntimeError::new("Invalid arguments, no arguments expected", access.access_file_pos.clone());

            return Err(err.with_type("InvalidArgumentError"));
        }

        return Ok(Value::int(self.data.len() as isize));
    }
}