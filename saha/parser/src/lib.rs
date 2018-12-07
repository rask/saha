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

#![allow(clippy::needless_return, clippy::redundant_field_names)]

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
        functions::{SahaCallable, UserFunction},
        objects::{MemberVisibility, BehaviorDefinition, ClassDefinition, Property, ObjProperties}
    },
    errors::{Error, ParseError},
    source::token::Token,
};

use crate::{
    parse_table::{ParseTable, BehaviorDefinition as PTBehaviorDefinition, ClassDefinition as PTClassDefinition},
    ast_parser::AstParser,
    root_parser::RootParser
};

/// Populate parsed constants to the global symbol table.
fn populate_constants(parse_table: &ParseTable) -> Result<(), ParseError> {
    let constants = parse_table.constants.to_owned();

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    st.set_constants(constants);

    return Ok(());
}

/// Populate functions to the global symbol table.
fn populate_functions(parse_table: &ParseTable) -> Result<(), ParseError> {
    let funcs = parse_table.functions.to_owned();

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    for (_, func) in funcs {
        let mut parser = AstParser::new(&func.body_tokens);

        let ast = parser.start_parse()?;

        let func = UserFunction {
            source_name: func.source_name,
            name: func.name,
            params: func.parameters,
            return_type: func.return_type,
            ast: ast,
            visibility: MemberVisibility::Public,
            is_static: false
        };

        st.add_function(Box::new(func));
    }

    return Ok(());
}

/// Populate the global symbol table with parsed behaviors.
fn populate_behaviors(parse_table: &ParseTable) -> Result<(), ParseError> {
    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    for (behname, parsed_behavior) in &parse_table.behaviors {
        let mut behavior_methods: HashMap<String, (_, _)> = HashMap::new();
        let behavior_name = parsed_behavior.name.clone();
        let behavior_plain_name = parsed_behavior.source_name.clone();

        for (mname, m) in &parsed_behavior.methods {
            let behmethod = (m.parameters.clone(), m.return_type.clone());

            behavior_methods.insert(mname.to_string(), behmethod);
        }

        let behavior_def: BehaviorDefinition = BehaviorDefinition {
            name: behavior_plain_name,
            fqname: behavior_name,
            methods: behavior_methods
        };

        st.behaviors.insert(behname.to_string(), behavior_def);
    }

    return Ok(());
}

/// Generate a collection of class property definitions for a class definition.
fn generate_class_properties(c: &PTClassDefinition) -> ObjProperties {
    let mut props: ObjProperties = HashMap::new();

    for pdef in c.properties.values() {
        props.insert(pdef.name.clone(), Property {
            name: pdef.name.clone(),
            prop_type: pdef.property_type.clone(),
            default: pdef.default.clone(),
            is_static: pdef.is_static,
            visibility: pdef.visibility.clone(),
            value: None
        });
    }

    return props;
}

/// Generate methods for a class definition.
fn generate_class_methods(c: &PTClassDefinition) -> Result<HashMap<String, Box<dyn SahaCallable>>, ParseError> {
    let mut methods: HashMap<String, Box<dyn SahaCallable>> = HashMap::new();

    for fndef in c.methods.values() {
        let mut parser = AstParser::new(&fndef.body_tokens);

        let ast = parser.start_parse()?;

        let func = UserFunction {
            source_name: fndef.source_name.clone(),
            name: fndef.name.clone(),
            params: fndef.parameters.clone(),
            return_type: fndef.return_type.clone(),
            ast: ast,
            visibility: fndef.visibility.clone(),
            is_static: fndef.is_static
        };

        methods.insert(fndef.source_name.clone(), Box::new(func));
    }

    return Ok(methods);
}

/// Check that classes implement their deifned behaviors correctly.
fn validate_class_implements(c: &PTClassDefinition, beh_defs: &HashMap<String, PTBehaviorDefinition>) -> Result<(), ParseError> {
    let c_impl = &c.implements;

    for i in c_impl {
        if !beh_defs.contains_key(i) {
            let err = ParseError::new(
                &format!("Invalid behavior implementation on `{}`, no behavior `{}` defined", c.name, i),
                Some(c.source_position.clone())
            );

            return Err(err);
        }

        let cbeh = beh_defs.get(i).unwrap();

        for (mname, method) in &cbeh.methods {
            if !c.methods.contains_key(mname) {
                let err = ParseError::new(
                    &format!("Invalid behavior implementation on `{}`, method `{}` defined in behavior `{}` not found in class", c.name, mname, cbeh.name),
                    Some(c.source_position.clone())
                );

                return Err(err);
            }

            let cmeth = &c.methods[mname];

            if cmeth != method {
                let err = ParseError::new(
                    &format!("Invalid behavior implementation on `{}`, method `{}` has mismatching definition from behavior `{}`", c.name, mname, cbeh.name),
                    Some(c.source_position.clone())
                );

                return Err(err);
            }
        }
    }

    return Ok(());
}

/// Populate parsed class definitions to the global symbol table.
fn populate_classes(parse_table: &ParseTable) -> Result<(), ParseError> {
    let classes = parse_table.classes.clone();
    let behaviors = &parse_table.behaviors;

    let mut st = SAHA_SYMBOL_TABLE.lock().unwrap();

    for (cname, c) in classes {
        validate_class_implements(&c, behaviors)?;

        let methods: HashMap<String, Box<dyn SahaCallable>> = generate_class_methods(&c)?;
        let props: ObjProperties = generate_class_properties(&c);

        let cdef = ClassDefinition {
            name: c.source_name.clone(),
            fqname: c.name.clone(),
            properties: props,
            implements: c.implements.clone(),
            type_params: c.type_params
        };

        st.classes.insert(cname.clone(), cdef);

        for m in methods.values() {
            st.add_method(&cname, &m);
        }
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
pub fn parse_tokens(tokens: &[Token]) -> Result<(), ParseError> {
    let mut parse_table = ParseTable::new();

    {
        let mut root_parser = RootParser::new(tokens, &mut parse_table);

        root_parser.start_parse()?;
    }

    populate_global_symbol_table(&parse_table)?;

    return Ok(());
}
