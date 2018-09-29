//! Saha Interpreter
//!
//! This is the Saha language interpreter.

extern crate saha_interpreter;

use std::process::exit;
use saha_interpreter::run;

/// Interpreter entrypoint.
fn main() {
    let result: i32 = run();

    exit(result);
}
