//! dictionary.rs
//!
//! Defines the internal global `Dict<T>` type object.

use std::{
    collections::HashMap,
    sync::Arc
};

use saha_lib::prelude::*;

use crate::stdlib::globals::option::SahaOption;

/// Create a new Dict instance.
pub fn new_instance(
    instref: InstRef,
    args: &SahaFunctionArguments,
    type_params: &[Box<SahaType>],
    additional_data: &SahaFunctionArguments,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 1 {
        let err = RuntimeError::new("`Dict` expects a single type parameter `T`", create_pos);

        return Err(err);
    }

    if !args.is_empty() {
        let err = RuntimeError::new("`Dict` expects no arguments", create_pos);

        return Err(err);
    }

    let initial_data = if additional_data.is_empty() {
        HashMap::new()
    } else {
        additional_data.clone()
    };

    let dict_inst = Box::new(SahaDict {
        param_type: type_params[0].clone(),
        data: initial_data.clone(),
        instref: instref
    });

    return Ok(dict_inst);
}

/// SahaDict is the core definition of the `Dict<T>` type in Saha.
#[derive(Clone, Debug)]
struct SahaDict {
    instref: InstRef,
    param_type: Box<SahaType>,
    pub data: HashMap<String, Value>
}

impl SahaObject for SahaDict {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref;
    }

    fn is_core_defined(&self) -> bool {
        return true;
    }

    fn get_class_name(&self) -> String {
        return "Dict".to_string();
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
            "Dict".to_string(),
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
            "insert" => self.insert(&args, access),
            "remove" => self.remove(&args, access),
            "get" => self.get(&args, access),
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
        return Box::new(self.data.clone().into_iter().map(|(key, val)| (Value::str(key), val)));
    }

    fn set_data_from_iter(&mut self, iterator: Box<Iterator<Item = (Value, Value)>>) {
        self.data = iterator.map(|(key, val)| (key.str.unwrap(), val)).collect();
    }
}

impl SahaDict {
    /// Parameters for the `insert` method.
    fn insert_params(&self) -> SahaFunctionParamDefs {
        let mut params = HashMap::new();

        params.insert("key".to_string(), FunctionParameter {
            name: "key".to_string(),
            param_type: Box::new(SahaType::Str),
            default: Value::void()
        });

        params.insert("value".to_string(), FunctionParameter {
            name: "value".to_string(),
            param_type: self.param_type.clone(),
            default: Value::void()
        });

        return params;
    }

    /// Insert a new value for a certain key.
    pub fn insert(&mut self, args: &SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.insert_params().validate_args(&args, access.access_file_pos)?;

        let key_str = args.get("key").unwrap().clone().str.unwrap();

        self.data.insert(key_str, args.get("value").unwrap().clone());

        return Ok(Value::void());
    }

    /// Params for the `remove` method.
    fn remove_params(&self) -> SahaFunctionParamDefs {
        let mut params = HashMap::new();

        params.insert("key".to_string(), FunctionParameter {
            name: "key".to_string(),
            param_type: Box::new(SahaType::Str),
            default: Value::void()
        });

        return params;
    }

    /// Remove an item from the dict.
    pub fn remove(&mut self, args: &SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.remove_params().validate_args(&args, access.access_file_pos)?;

        let key_str = args["key"].clone().str.unwrap();

        self.data.remove(&key_str);

        return Ok(Value::void());
    }

    /// Params for the `get` method.
    fn get_params(&self) -> SahaFunctionParamDefs {
        let mut params = HashMap::new();

        params.insert("key".to_string(), FunctionParameter {
            name: "key".to_string(),
            param_type: Box::new(SahaType::Str),
            default: Value::void()
        });

        return params;
    }

    /// Get an item from the dict. Returns SahaOption.
    pub fn get(&mut self, args: &SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.get_params().validate_args(&args, access.access_file_pos)?;

        let key_str = args["key"].clone().str.unwrap();

        let opt_obj: Box<dyn SahaObject>;
        let opt_instref = crate::utils::get_new_instref();

        if !self.data.contains_key(&key_str) {
            opt_obj = SahaOption::new_none(
                opt_instref,
                self.param_type.clone()
            );
        } else {
            let val = self.data[&key_str].clone();

            opt_obj = SahaOption::new_some(
                opt_instref,
                val.clone(),
                self.param_type.clone()
            );
        }

        crate::utils::add_instance_to_symbol_table(opt_instref, opt_obj);

        return Ok(Value::obj(opt_instref));
    }
}
