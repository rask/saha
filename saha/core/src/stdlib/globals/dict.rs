//! dictionary.rs
//!
//! Defines the internal global `Dict<T>` type object.

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

use crate::stdlib::globals::result::SahaResult;

/// Create a new Dict instance.
pub fn new_instance(
    instref: InstRef,
    args: SahaFunctionArguments,
    type_params: &Vec<Box<SahaType>>,
    additional_data: SahaFunctionArguments,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 1 {
        let err = RuntimeError::new("`Dict` expects a single type parameter `T`", create_pos);

        return Err(err);
    }

    if args.len() > 0 {
        let err = RuntimeError::new("`Dict` expects no arguments", create_pos);

        return Err(err);
    }

    let initial_data: HashMap<String, Value>;

    if additional_data.is_empty() {
        initial_data = HashMap::new();
    } else {
        initial_data = additional_data;
    }

    let dict_inst = Box::new(SahaDict {
        param_type: type_params[0].clone(),
        data: initial_data,
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
        return self.instref.clone();
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

    fn get_full_method_name(&mut self, method_name: &str) -> String {
        unimplemented!()
    }

    fn get_method_ref(&mut self, method_name: &str) -> Result<Arc<Box<dyn SahaCallable>>, RuntimeError> {
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

        let list_type = self.get_type_params()[0].1.clone();

        match access.member_name as &str {
            "insert" => self.insert(args, access),
            "remove" => self.remove(args, access),
            "get" => self.get(args, access),
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
    pub fn insert(&mut self, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
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
    pub fn remove(&mut self, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.remove_params().validate_args(&args, access.access_file_pos)?;

        let key_str = args.get("key").unwrap().clone().str.unwrap();

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

    /// Get an item from the dict. Returns result, where success is the value.
    pub fn get(&mut self, args: SahaFunctionArguments, access: AccessParams) -> SahaCallResult {
        self.get_params().validate_args(&args, access.access_file_pos)?;

        let key_str = args.get("key").unwrap().clone().str.unwrap();

        let res_obj: Box<dyn SahaObject>;
        let res_instref = crate::utils::get_new_instref();

        if self.data.contains_key(&key_str) == false {
            res_obj = SahaResult::new_failure(
                res_instref,
                Value::str(format!("No key `{}` defined", key_str)),
                self.param_type.clone(),
                Box::new(SahaType::Str)
            );
        } else {
            let val = self.data.get(&key_str).unwrap();

            res_obj = SahaResult::new_success(
                res_instref,
                val.clone(),
                self.param_type.clone(),
                Box::new(SahaType::Str)
            );
        }

        crate::utils::add_instance_to_symbol_table(res_instref, res_obj);

        return Ok(Value::obj(res_instref));
    }
}
