//! Saha Symbol Table
//!
//! The Saha symbol table is the main symbol and reference storage when running
//! a Saha application. It is global and under mutex restrictions.
//!
//! All declarations and globals are stored in the symbol table.

use std::collections::HashMap;
use uuid::Uuid;
use ::types::objects::SahaObject;

/// UUID as bytes
pub type InstRef = [u8; 16];

/// Symbol table, stores global parsed declarations and definitions, in addition
/// to references to things that should be available globally.
pub struct SymbolTable {
    constants: HashMap<String, ()>,
    functions: HashMap<String, ()>,
    behaviors: HashMap<String, ()>,
    classes: HashMap<String, ()>,
    instances: HashMap<InstRef, Box<SahaObject>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        return SymbolTable {
            constants: HashMap::new(),
            functions: HashMap::new(),
            behaviors: HashMap::new(),
            classes: HashMap::new(),
            instances: HashMap::new(),
        };
    }

    fn get_new_uuid_bytes() -> InstRef {
        Uuid::new_v4().as_bytes().to_owned()
    }
}