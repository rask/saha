//! Saha Interpreter Library
//!
//! Provides the main interpreter logic and clue code, tying together the
//! language tooling to make it usable.

#![allow(clippy::needless_return, clippy::redundant_field_names)]

extern crate rustc_version;
extern crate saha_tokenizer;
extern crate saha_parser;
extern crate saha_lib;
extern crate saha_core;
extern crate structopt;

mod cli;
mod errors;

use std::{
    collections::HashMap,
    env::current_dir,
    ffi::OsString,
    path::PathBuf,
    io::{self, Read}
};

use saha_lib::{
    source::files::FilePosition,
    errors::{Error, ParseError, RuntimeError},
    types::functions::{SahaCallable, UserFunction}
};

use saha_tokenizer::{tokenize_file, tokenize_raw_source_code};
use saha_parser::parse_tokens;
use saha_core::register_core;

use crate::errors::{StartupError, StartupResult};

/// Validate if the given interpreter entrypoint file is a valid file for usage.
///
/// Following validation is done:
///
/// -   File should exist in the filesystem
/// -   File name should end with `.saha`
fn validate_interpreter_entrypoint(entrypoint: &PathBuf) -> StartupResult {
    if !entrypoint.exists() {
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

    if !entrypoint.is_absolute() {
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

    let tokenized_source = tokenize_file(&entrypoint)?;

    // Parse tokens into declarations and definitions into the global symbol table.
    parse_tokens(&tokenized_source)?;

    return Ok(());
}

/// Load Saha core, meaning stdlib, extensions, and such.
fn load_saha_core() -> Result<(), StartupError> {
    register_core();

    return Ok(());
}

/// Run the `main()` of our input source code.
fn run_saha_main() -> Result<i32, RuntimeError> {
    let mainfn: UserFunction;

    {
        let st = saha_lib::SAHA_SYMBOL_TABLE.lock().unwrap();
        let main_callable = st.functions["pkg.main"].clone();

        mainfn = (*main_callable).as_userfunction().clone();
    }

    let main_result = mainfn.call(HashMap::new(), None, Vec::new(), Some(FilePosition::unknown()))?;

    // other checks should enforce that we're working with an integer
    let return_code = main_result.int.unwrap_or(-255);

    return Ok(return_code as i32);
}

/// Attempt to read Saha source code piped in through the STDIN.
fn try_to_read_source_from_stdin() -> Option<String> {
    let mut buffer = String::new();
    let read_result = io::stdin().read_to_string(&mut buffer);

    if read_result.is_err() {
        return None;
    }

    if buffer.len() > 0 {
        return Some(buffer);
    }

    return None;
}

/// Run the interpreter for simple scripts piped in through STDIN.
fn run_stdin_interpreter(source: String) -> i32 {
    let tokenized_source = tokenize_raw_source_code(source);

    if tokenized_source.is_err() {
        eprintln!("{}", tokenized_source.err().unwrap().format());
        return 1;
    }

    let tokenized_source = tokenized_source.ok().unwrap();

    // Parse tokens into declarations and definitions into the global symbol table.
    let parse_result = parse_tokens(&tokenized_source);

    if parse_result.is_err() {
        eprintln!("{}", parse_result.err().unwrap().format());
        return 1;
    }

    // At this point we should have core and extensions loaded, and we also have read, tokenized,
    // and parsed our Saha source code. The symbol table is ready and now we just need to call the
    // application main().
    let run_result = run_saha_main();

    if run_result.is_err() {
        eprintln!("{}", run_result.err().unwrap().format());
        return 1;
    }

    return run_result.ok().unwrap();
}

/// Run the interpreter.
fn run_interpreter(args: &cli::InterpreterArgs) -> i32 {
    let core_loaded = load_saha_core();

    if core_loaded.is_err() {
        eprintln!("{}", core_loaded.err().unwrap().format());
        return 1;
    }

    let stdin_source = try_to_read_source_from_stdin();

    // Got input via STDIN, work with that and disregard the rest.
    if stdin_source.is_some() {
        return run_stdin_interpreter(stdin_source.unwrap());
    }

    if args.entrypoint.to_str().unwrap() == "" {
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

    // At this point we should have core and extensions loaded, and we also have read, tokenized,
    // and parsed our Saha source code. The symbol table is ready and now we just need to call the
    // application main().
    let run_result = run_saha_main();

    if run_result.is_err() {
        eprintln!("{}", run_result.err().unwrap().format());
        return 1;
    }

    return run_result.ok().unwrap();
}

/// Get the rustc version on which this thing is built with. This information
/// can be used to verify the Rust ABI this version of Saha was built with.
fn get_rustc_version() -> String {
    let version = rustc_version::version();

    match version {
        Ok(v) => v.to_string(),
        Err(_) => "unknown".to_string()
    }
}

/// Display version information.
fn run_version() -> i32 {
    println!("Saha {}", env!("CARGO_PKG_VERSION"));
    println!("ABI: rustc-{}", get_rustc_version());
    return 0;
}

/// Run the interpreter.
///
/// First we parse the CLI arguments, then we validate those. Next we pass the
/// interpreter entrypoint to a tokenizer, which in turn is parsed into
/// declarations and those are refined into sub-ASTs which are used to make the
/// interpretation itself work. After this we load the core, after which we are
/// ready to run interpretatextern crate saha_interpreter;ion.
///
/// We do parsing before loading core to make it fail faster in case of parse
/// errors or other logical errors that developers in userland may have
/// introduced in their code.
///
/// Returns an `i32` for exit coding purposes, but will catch any errors that
/// occur during any of the phases mentioned above.
pub fn run() -> i32 {
    let args: cli::InterpreterArgs = cli::get_cli_arguments();

    if args.version {
        return run_version();
    } else {
        return run_interpreter(&args);
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
