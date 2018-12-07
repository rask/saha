//! option.rs
//!
//! Anything related to the `Option<T>` type used for error and result
//! management in Saha. Very similar to the `Option` type in Rust.

use std::sync::Arc;

use saha_lib::prelude::*;

/// Create a new Option instance.
pub fn new_instance(
    instref: InstRef,
    args: &SahaFunctionArguments,
    type_params: &[Box<SahaType>],
    additional_data: &SahaFunctionArguments,
    create_pos: Option<FilePosition>
) -> Result<Box<dyn SahaObject>, RuntimeError> {
    if type_params.len() != 2 {
        let err = RuntimeError::new("`Option` expects a type parameter `T`", create_pos);

        return Err(err);
    }

    if !args.is_empty() {
        let err = RuntimeError::new("`Option` expects no arguments", create_pos);

        return Err(err);
    }

    // FIXME type param validation

    let initial_value: Value;
    let is_initially_some: bool;

    if additional_data.is_empty() {
        initial_value = Value::void();
        is_initially_some = false;
    } else {
        unimplemented!()
    }

    let option_inst = Box::new(SahaOption {
        option_type: type_params[0].clone(),
        option_value: initial_value,
        is_some: is_initially_some,
        instref: instref
    });

    return Ok(option_inst);
}

/// SahaOption is the option type generic class for optional outcomes in Saha.
#[derive(Clone)]
pub struct SahaOption {
    instref: InstRef,
    pub option_type: Box<SahaType>,
    pub option_value: Value,
    pub is_some: bool
}

impl SahaObject for SahaOption {
    fn get_instance_ref(&self) -> InstRef {
        return self.instref;
    }

    fn is_core_defined(&self) -> bool {
        return true;
    }

    fn get_class_name(&self) -> String {
        return "Option".to_string();
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
        return vec![
            ('T', self.option_type.clone())
        ];
    }

    fn get_named_type(&self) -> Box<SahaType> {
        return Box::new(SahaType::Name(
            "Option".to_string(),
            vec![self.option_type.clone()]
        ));
    }

    fn call_member(&mut self, access: AccessParams, _args: SahaFunctionArguments) -> SahaCallResult {
        if access.is_static_access {
            match access.member_name as &str {
                _ => Err(RuntimeError::new(
                    &format!("No static method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                    access.access_file_pos.clone()
                ))
            }
        } else {
            // FIXME arg validation missing
            match access.member_name as &str {
                "isSome" => self.is_some(),
                "isNone" => self.is_none(),
                "unwrap" => {
                    if !self.is_some {
                        let err = RuntimeError::new(
                            "Attempted to unwrap a none value of an Option",
                            access.access_file_pos.clone()
                        );

                        return Err(err);
                    }

                    Ok(self.option_value.clone())
                },
                _ => {
                    Err(RuntimeError::new(
                        &format!("No method `{}` defined for `{}`", access.member_name, self.get_class_name()),
                        access.access_file_pos.clone()
                    ))
                }
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
        unimplemented!()
    }

    fn set_data_from_iter(&mut self, _iterator: Box<Iterator<Item = (Value, Value)>>) {
        unimplemented!()
    }
}

impl SahaOption {
    /// Create a new some typed SahaOption.
    pub fn new_some(instref: InstRef, value: Value, option_type: Box<SahaType>) -> Box<dyn SahaObject> {
        return Box::new(SahaOption {
            option_type: option_type,
            option_value: value,
            is_some: true,
            instref: instref
        });
    }

    /// Create a new none typed SahaOption.
    pub fn new_none(instref: InstRef, option_type: Box<SahaType>) -> Box<dyn SahaObject> {
        return Box::new(SahaOption {
            option_type: option_type,
            option_value: Value::void(),
            is_some: false,
            instref: instref
        });
    }

    /// Does this option contain a value?
    pub fn is_some(&self) -> SahaCallResult {
        return Ok(Value::bool(self.is_some));
    }

    /// Is this option empty?
    pub fn is_none(&self) -> SahaCallResult {
        return Ok(Value::bool(!self.is_some));
    }
}