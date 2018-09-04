//! Saha Interpreter Library
//!
//! Provides the main interpreter logic and clue code, tying together the
//! language tooling to make it usable.

extern crate saha_tokenizer;
extern crate saha_parser;
extern crate saha_lib;
extern crate saha_core;
#[macro_use]
extern crate structopt;

mod cli;
mod errors;

use std::path::PathBuf;
use saha_lib::errors::Error;
use errors::{StartupError, StartupResult};

/// Validate if the given interpreter entrypoint file is a valid file for usage.
///
/// Following validation is done:
///
/// -   File should exist in the filesystem
/// -   File name should end with `.saha`
fn validate_interpreter_entrypoint(entrypoint: PathBuf) -> StartupResult {
    if entrypoint.exists() == false {
        return Err(StartupError::new("Invalid entrypoint file, does not exist"));
    }

    if entrypoint.ends_with(".saha") == false {
        return Err(StartupError::new("Invalid entrypoint file, not a Saha file ending in `.saha`"));
    }

    return Ok(());
}

/// Run the interpreter.
///
/// First we parse the CLI arguments, then we validate those. Next we pass the
/// interpreter entrypoint to a tokenizer, which in turn is parsed into
/// declarations and those are refined into sub-ASTs which are used to make the
/// interpretation itself work. After this we load the core, after which we are
/// ready to run interpretation.
///
/// We do parsing before loading core to make it fail faster in case of parse
/// errors or other logical errors that developers in userland may have
/// introduced in their code.
///
/// Returns an `i32` for exit coding purposes, but will catch any errors that
/// occur during any of the phases mentioned above.
pub fn run() -> i32 {
    let args: cli::InterpreterArgs = cli::get_cli_arguments();

    let entrypoint_validation_result: StartupResult = validate_interpreter_entrypoint(args.entrypoint);

    if entrypoint_validation_result.is_err() {
        eprintln!("{}", entrypoint_validation_result.err().unwrap().get_message());
        return 1;
    }

    return 0;
}

#[cfg(test)]
mod tests {
    // TODO
}
