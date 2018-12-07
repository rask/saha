//! list.rs
//!
//! Defines the internal global `List<T>` type object.

use std::{
    collections::HashMap,
    sync::Arc
};

use saha_lib::prelude::*;

/// Create a new List instance.
pub fn new_instance(
    instref: InstRef,
    args: &SahaFunctionArguments,
    type_params: &[Box<SahaType>],
    additional_data: &SahaFunctionArguments,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 1 {
        let err = RuntimeError::new("`List` expects a single type parameter `T`", create_pos);

        return Err(err);
    }

    if !args.is_empty() {
        let err = RuntimeError::new("`List` expects no arguments", create_pos);

        return Err(err);
    }

    let initial_data = if additional_data.is_empty() {
        vec![]
    } else {
        additional_data.iter().map(|(_, i)| i.clone()).collect()
    };

    let list_inst = Box::new(SahaList {
        param_type: type_params[0].clone(),
        data: initial_data,
        instref: instref,
        cursor_position: 0
    });

    return Ok(list_inst);
}

/// SahaList is the core definition of the `List<T>` type in Saha.
#[derive(Clone, Debug)]
struct SahaList {
    instref: InstRef,
    param_type: Box<SahaType>,
    pub data: Vec<Value>,
    pub cursor_position: usize,
}

impl SahaObject for SahaList {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref;
    }

    fn is_core_defined(&self) -> bool {
        return true;
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

    fn get_full_method_name(&mut self, _method_name: &str) -> String {
        unimplemented!()
    }

    fn get_method_ref(&mut self, _method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError> {
        unimplemented!()
    }

    fn get_type_params(&self) -> Vec<(char, Box<SahaType>)> {
        return vec![('T', self.param_type.clone())];
    }

    fn get_named_type(&self) -> Box<SahaType> {
        return Box::new(SahaType::Name(
            "List".to_string(),
            vec![self.param_type.clone()]
        ));
    }

    fn call_member(&mut self, access: AccessParams, args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            return Err(RuntimeError::new(
                &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                access.access_file_pos.clone()
            ));
        }

        match access.member_name as &str {
            "push" => self.push(&args, access),
            "count" => self.count(&args, access),
            _ => {
                return Err(RuntimeError::new(
                    &format!("No method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                    access.access_file_pos.clone()
                ));
            }
        }
    }

    fn access_property(&self, _access: AccessParams) -> SahaCallResult {
        unimplemented!()
    }

    fn mutate_property(&mut self, _access: AccessParams, _new_value: Value) -> SahaCallResult {
        unimplemented!()
    }

    fn box_clone(&self) -> Box<dyn SahaObject> {
        return Box::new(self.clone());
    }

    fn into_iter(&self) -> Box<Iterator<Item = (Value, Value)>> {
        return Box::new(self.data.clone().into_iter().enumerate().map(|(idx, val)| (Value::int(idx as isize), val)));
    }

    fn set_data_from_iter(&mut self, iterator: Box<Iterator<Item = (Value, Value)>>) {
        self.data = iterator.map(|(_, val)| val).collect();
    }
}

impl SahaList {
    /// Get function parameter definition for the List::push method.
    fn push_params(&self) -> SahaFunctionParamDefs {
        let mut params: SahaFunctionParamDefs = HashMap::new();

        params.insert("value".to_string(), FunctionParameter {
            name: "value".to_string(),
            param_type: self.param_type.clone(),
            default: Value::void()
        });

        return params;
    }

    /// The List::push "method".
    pub fn push(&mut self, args: &SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.push_params().validate_args(&args, access.access_file_pos)?;

        let pushed_val = args.get("value").unwrap();

        self.data.push(pushed_val.clone());

        return Ok(Value::void());
    }

    /// The List::count "method".
    pub fn count(&self, args: &SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        let params: SahaFunctionParamDefs = HashMap::new(); // no params

        params.validate_args(&args, access.access_file_pos)?;

        let count = self.data.len();

        return Ok(Value::int(count as isize));
    }
}