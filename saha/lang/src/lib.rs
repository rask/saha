//! Saha Interpreter Library
//!
//! Provides the main interpreter logic and clue code, tying together the
//! language tooling to make it usable.

extern crate rustc_version;
extern crate saha_tokenizer;
extern crate saha_parser;
extern crate saha_lib;
extern crate saha_core;
#[macro_use]
extern crate structopt;

mod cli;
mod errors;

use std::{
    env::current_dir,
    ffi::OsString,
    path::PathBuf
};

use saha_lib::errors::{Error, ParseError};
use saha_tokenizer::tokenize;
use errors::{StartupError, StartupResult};

/// Validate if the given interpreter entrypoint file is a valid file for usage.
///
/// Following validation is done:
///
/// -   File should exist in the filesystem
/// -   File name should end with `.saha`
fn validate_interpreter_entrypoint(entrypoint: &PathBuf) -> StartupResult {
    if entrypoint.exists() == false {
        return Err(StartupError::new("Invalid entrypoint file, does not exist", None));
    }

    if entrypoint.extension().unwrap_or(&OsString::new()) != "saha" {
        return Err(StartupError::new("Invalid entrypoint file, not a Saha file ending in `.saha`", None));
    }

    return Ok(());
}

/// Takes a source file path and tokenizes the contents.
fn parse_saha_source(args: &cli::InterpreterArgs) -> Result<(), ParseError> {
    let mut entrypoint = args.entrypoint.to_owned();

    if entrypoint.is_absolute() == false {
        let dir = current_dir();

        match dir {
            Ok(mut d) => {
                d.push(entrypoint);
                entrypoint = d;
            },
            Err(_) => {
                return Err(ParseError::new("Unable to parse current working directory for entrypoint", None))
            }
        };
    }

    let tokenized_source = tokenize(&entrypoint)?;

    return Ok(());
}

/// Run the interpreter.
fn run_interpreter(args: cli::InterpreterArgs) -> i32 {
    if (args.entrypoint.to_str().unwrap() == "") {
        eprintln!("{}", StartupError::new("Please provide a Saha source file", None).format());
        return 1;
    }

    let entrypoint_validation_result: StartupResult = validate_interpreter_entrypoint(&args.entrypoint);

    if entrypoint_validation_result.is_err() {
        eprintln!("{}", entrypoint_validation_result.err().unwrap().format());
        return 1;
    }

    let parse_result = parse_saha_source(&args);

    if parse_result.is_err() {
        eprintln!("{}", parse_result.err().unwrap().format());
        return 1;
    }

    return 0;
}

/// Get the rustc version on which this thing is built with. This information
/// can be used to verify the Rust ABI this version of Saha was built with.
fn get_rustc_version() -> String {
    let version = rustc_version::version();

    match version {
        Ok(v) => v.to_string(),
        Err(e) => "unknown".to_string()
    }
}

/// Display version information.
fn run_version() -> i32 {
    println!("Saha {}", env!("CARGO_PKG_VERSION"));
    println!("    ABI: rustc-{}", get_rustc_version());
    return 0;
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

    if args.version == true {
        return run_version();
    } else {
        return run_interpreter(args);
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
