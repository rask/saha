//! Saha Symbol Table
//!
//! The Saha symbol table is the main symbol and reference storage when running
//! a Saha application. It is global and under mutex restrictions.
//!
//! All declarations and globals are stored in the symbol table.

use std::collections::HashMap;
use uuid::Uuid;

use ::{
    errors::{Error, ParseError},
    types::{
        Value,
        functions::SahaCallable,
        objects::SahaObject
    }
};

/// UUID as bytes
pub type InstRef = [u8; 16];

/// Symbol table, stores global parsed declarations and definitions, in addition
/// to references to things that should be available globally.
pub struct SymbolTable {
    constants: HashMap<String, Value>,
    functions: HashMap<String, Box<SahaCallable>>,
    behaviors: HashMap<String, ()>,
    classes: HashMap<String, ()>,
    instances: HashMap<InstRef, Box<SahaObject>>,
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
    pub fn add_function(&mut self, func: Box<SahaCallable>) {
        unimplemented!()
    }

    /// Get a new random UUID types type instance reference.
    fn get_new_uuid_bytes() -> InstRef {
        Uuid::new_v4().as_bytes().to_owned()
    }
}