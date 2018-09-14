//! Parse Table
//!
//! Stores declarations and definitions for the root parser. These are later
//! used to generate the Saha symbol table.

use std::collections::HashMap;

pub struct ParseTable {
    functions: HashMap<String, ()>,
    constants: HashMap<String, ()>,
    classes: HashMap<String, ()>,
    behaviors: HashMap<String, ()>,
}

impl ParseTable {
    pub fn new() -> ParseTable {
        return ParseTable {
            functions: HashMap::new(),
            constants: HashMap::new(),
            classes: HashMap::new(),
            behaviors: HashMap::new(),
        };
    }
}