//! Saha Parser
//!
//! The parser takes in tokens which have been parsed by the Saha tokenizer, and
//! generates a machine readable application structure from them.
//!
//! First we parse the application outline with class, function, behavior, and
//! constant definitions in place.
//!
//! Then we hop into each class method and function and parse the abstract
//! syntax tree for all of them.
//!
//! After parsing is done we have a ready to interpret application.

extern crate saha_lib;
extern crate noisy_float;

mod parse_table;
mod parser;
mod root_parser;
mod ast_parser;

use std::collections::HashMap;

use saha_lib::{
    SAHA_SYMBOL_TABLE,
    types::{
        Value, SahaType,
        functions::{SahaCallable, UserFunction},
        objects::{ClassDefinition, Property, ObjProperties}
    },
    errors::ParseError,
    source::token::Token,
};

use crate::{
    parse_table::ParseTable,
    ast_parser::AstParser,
    root_parser::RootParser
};

fn populate_constants(parse_table: &ParseTable) -> Result<(), ParseError> {
    let constants = parse_table.constants.to_owned();

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    st.set_constants(constants);

    return Ok(());
}

fn populate_functions(parse_table: &ParseTable) -> Result<(), ParseError> {
    let funcs = parse_table.functions.to_owned();

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    for (fname, func) in funcs {
        let mut parser = AstParser::new(&func.body_tokens);

        let ast = parser.start_parse()?;

        let func = UserFunction {
            source_name: func.source_name,
            name: func.name,
            params: func.parameters,
            return_type: func.return_type,
            ast: ast,
            is_public: true,
            is_static: false
        };

        st.add_function(Box::new(func));
    }

    return Ok(());
}

fn populate_behaviors(parse_table: &ParseTable) -> Result<(), ParseError> {
    return Ok(());
}

fn populate_classes(parse_table: &ParseTable) -> Result<(), ParseError> {
    let classes = parse_table.classes.clone();

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    for (cname, c) in classes {
        // FIXME handle methods and props here
        let mut methods: HashMap<String, Box<dyn SahaCallable>> = HashMap::new();
        let mut props: ObjProperties = HashMap::new();

        let cdef = ClassDefinition {
            name: c.source_name.clone(),
            fqname: c.name.clone(),
            methods: methods,
            properties: props,
            implements: c.implements.clone()
        };

        st.classes.insert(cname, cdef);
    }

    return Ok(());
}

/// Take a parse table and populate the Saha symbol table with the definitions
/// in it.
fn populate_global_symbol_table(parse_table: &ParseTable) -> Result<(), ParseError> {
    populate_constants(&parse_table)?;
    populate_functions(&parse_table)?;
    populate_behaviors(&parse_table)?;
    populate_classes(&parse_table)?;

    return Ok(());
}

/// Parse a collection of tokens into a declaration table and ASTs.
pub fn parse_tokens(tokens: &Vec<Token>) -> Result<(), ParseError> {
    let mut parse_table = ParseTable::new();

    {
        let mut root_parser = RootParser::new(&tokens, &mut parse_table);

        root_parser.start_parse()?;
    }

    populate_global_symbol_table(&parse_table)?;

    return Ok(());
}
