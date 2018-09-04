//! Saha Interpreter
//!
//! This is the Saha language interpreter.

extern crate saha_lang;

use std::process::exit;
use saha_lang::run;

/// Interpreter entrypoint.
fn main() {
    let result: i32 = run();

    exit(result);
}
