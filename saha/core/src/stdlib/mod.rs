//! Saha standard library
//!
//! The Saha stdlib contains generic language features that are usable in all
//! Saha code bases.

mod globals;

use saha_lib::types::functions::CoreFunction;

pub fn register_stdlib() {
    let mut funcs: Vec<(String, CoreFunction)> = Vec::new();

    funcs.append(&mut globals::print::get_saha_functions());

    {
        let mut st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

        for (fnname, fnitem) in funcs {
            st.functions.insert(fnname, Box::new(fnitem));
        }
    }

    register_list_class();
    register_result_class();
}

/// Register the stdlib List type.
fn register_list_class() {
    let mut st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

    st.core_classes.insert("List".to_string(), globals::list::new_instance);
}

/// Register the stdlib Result type.
fn register_result_class() {
    let mut st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

    st.core_classes.insert("Result".to_string(), globals::result::new_instance);
}
