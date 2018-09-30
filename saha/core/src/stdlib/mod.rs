//! Saha standard library
//!
//! The Saha stdlib contains generic language features that are usable in all
//! Saha code bases.

mod globals;

use saha_lib::types::functions::CoreFunction;

pub fn register_stdlib() {
    let mut funcs: Vec<(String, CoreFunction)> = Vec::new();

    funcs.append(&mut globals::get_saha_functions());

    let mut st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();

    for (fnname, fnitem) in funcs {
        st.functions.insert(fnname, Box::new(fnitem));
    }
}