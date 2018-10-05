//! Parse Table
//!
//! Stores declarations and definitions for the root parser. These are later
//! used to generate the Saha symbol table.

use std::collections::HashMap;

use saha_lib::{
    source::{
        files::FilePosition,
        token::Token,
    },
    types::{
        Value,
        SahaType,
        objects::MemberVisibility,
        functions::SahaFunctionParamDefs
    }
};

/// Function defintion, from which a callable can be built from.
#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub source_name: String,
    pub source_position: FilePosition,
    pub return_type: SahaType,
    pub body_tokens: Vec<Token>,
    pub visibility: MemberVisibility,
    pub is_static: bool,
    pub parameters: SahaFunctionParamDefs
}

/// A property definition.
#[derive(Clone)]
pub struct PropertyDefinition {
    pub name: String,
    pub source_position: FilePosition,
    pub visibility: MemberVisibility,
    pub is_static: bool,
    pub property_type: SahaType,
    pub default: Value
}

/// Class definition blueprint. Used to generate class definitions to a symbol
/// table.
#[derive(Clone)]
pub struct ClassDefinition {
    pub name: String,
    pub source_name: String,
    pub source_position: FilePosition,
    pub properties: HashMap<String, PropertyDefinition>,
    pub methods: HashMap<String, FunctionDefinition>,
    pub implements: Vec<String>,
}

/// Behavior definitions, used to build actual behaviors.
#[derive(Clone)]
pub struct BehaviorDefinition {
    pub name: String,
    pub source_name: String,
    pub source_position: FilePosition,
    pub methods: HashMap<String, FunctionDefinition>
}

/// Intermediate parse table, contains "blueprints" for root level declarations.
pub struct ParseTable {
    pub functions: HashMap<String, FunctionDefinition>,
    pub constants: HashMap<String, Value>,
    pub classes: HashMap<String, ClassDefinition>,
    pub behaviors: HashMap<String, BehaviorDefinition>,
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

impl PartialEq for FunctionDefinition {
    fn eq(&self, other: &FunctionDefinition) -> bool {
        let name_match = self.name == other.name;
        let params_match = self.parameters == other.parameters;
        let ret_match = self.return_type == other.return_type;
        let static_match = self.is_static == other.is_static;
        let pub_match = self.visibility == other.visibility;

        return name_match && params_match && ret_match && static_match && pub_match;
    }
}

impl Eq for FunctionDefinition {}