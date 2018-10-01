//! Saha Symbol Table
//!
//! The Saha symbol table is the main symbol and reference storage when running
//! a Saha application. It is global and under mutex restrictions.
//!
//! All declarations and globals are stored in the symbol table.

use std::collections::HashMap;
use uuid::Uuid;

use crate::{
    errors::{Error, ParseError, RuntimeError},
    source::files::FilePosition,
    types::{
        Value,
        functions::{SahaCallable, SahaFunctionArguments},
        objects::{SahaObject, ClassDefinition, BehaviorDefinition}
    }
};

/// UUID as bytes
pub type InstRef = [u8; 16];

/// Symbol table, stores global parsed declarations and definitions, in addition
/// to references to things that should be available globally.
pub struct SymbolTable {
    pub constants: HashMap<String, Value>,
    pub functions: HashMap<String, Box<dyn SahaCallable>>,
    pub behaviors: HashMap<String, BehaviorDefinition>,
    pub classes: HashMap<String, ClassDefinition>,
    pub instances: HashMap<InstRef, Box<dyn SahaObject>>,
}

impl SymbolTable {
    /// Return a new and empty symbol table.
    pub fn new() -> SymbolTable {
        return SymbolTable {
            constants: HashMap::new(),
            functions: HashMap::new(),
            behaviors: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
        };
    }

    /// Set the symbol table constants collection.
    pub fn set_constants(&mut self, constants: HashMap<String, Value>) {
        self.constants = constants;
    }

    /// Add a new function/callable.
    pub fn add_function(&mut self, func: Box<dyn SahaCallable>) {
        let fn_name = func.get_name().clone();

        // FIXME prevent overrides
        self.functions.insert(fn_name, func);
    }

    /// Insert a new object instance to the symbol table, and then return the
    /// instref value.
    pub fn create_object_instance(&mut self, class_name: String, args: SahaFunctionArguments, create_pos: &Option<FilePosition>) -> Result<Value, RuntimeError> {
        let def: Option<&ClassDefinition> = self.classes.get(&class_name);

        if def.is_none() {
            let err = RuntimeError::new(&format!("Cannot create instance of unknown class `{}`", class_name), create_pos.to_owned());

            return Err(err.with_type("TypeError"));
        }

        let def = def.unwrap();

        let instref = Self::get_new_uuid_bytes();

        let inst: Box<dyn SahaObject> = def.create_new_instance(instref, args, create_pos)?;

        self.instances.insert(instref, inst);

        return Ok(Value::obj(instref));
    }

    /// Get a new random UUID types type instance reference.
    fn get_new_uuid_bytes() -> InstRef {
        Uuid::new_v4().as_bytes().to_owned()
    }
}