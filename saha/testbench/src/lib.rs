//! Saha Testbench Library
//!
//! The Saha Testbench is used to run naive end-to-end tests against a compiled
//! Saha interpreter binary.
//!
//! Input a testbench format file, with desired output and exit code for a
//! certain piece of source code, then the testbench will assert the source code
//! and output match, or the test fails.

#![allow(clippy::needless_return, clippy::redundant_field_names)]

use std::{
    collections::HashMap,
    path::PathBuf,
    io::prelude::*,
    fs::File
};

use glob;
use regex;

mod cli;
mod testcase;
mod testrunner;

use crate::testcase::{
    TestCaseCollection,
    parse_files_to_test_cases
};

use crate::testrunner::{
    TestRunner,
    TestResult
};

/// Load the test case file contents.
fn load_file_collection_contents(files: &[PathBuf]) -> HashMap<PathBuf, Result<String, ()>> {
    let mut file_content_buffer = String::new();

    return files.iter().map(|p| {
        file_content_buffer = String::new();

        let result = File::open(p).unwrap().read_to_string(&mut file_content_buffer);

        if result.is_err() {
            (p.clone(), Err(()))
        } else {
            (p.clone(), Ok(file_content_buffer.clone()))
        }
    }).collect::<HashMap<PathBuf, _>>();
}

/// Load either a single test case file, or glob a directory for test case
/// files. Return them as a collection for the test runner.
fn load_test_case_files(root: PathBuf) -> Result<Vec<PathBuf>, ()> {
    let mut test_files: Vec<PathBuf> = Vec::new();

    if root.is_dir() {
        let test_files_maybe = glob::glob(&(String::from(root.to_str().unwrap()) + "/**/*.sahatest"));

        if test_files_maybe.is_err() {
            eprintln!("Could not load test files, directory parse error");

            return Err(());
        }

        test_files = test_files_maybe.ok().unwrap().map(|p| p.unwrap()).collect::<Vec<_>>();
    } else if root.is_file() {
        test_files.push(root);
    } else {
        eprintln!("Could not load test file, is it a `.sahatest` file and readable?");

        return Err(());
    }

    return Ok(test_files);
}

/// Run the library.
///
/// Returns an integer for exit status. If `--allow-failure` was passed in as an
/// argument, we exit with `0` in any case.
pub fn run() -> i32 {
    let args: cli::CliArgs = cli::get_cli_arguments();

    let test_file = args.test;
    let test_files = load_test_case_files(test_file);

    if test_files.is_err() {
        return 1;
    }

    let test_files = test_files.ok().unwrap();

    if test_files.is_empty() {
        return 0;
    }

    let test_files = load_file_collection_contents(&test_files);

    let test_cases: TestCaseCollection = parse_files_to_test_cases(test_files);

    println!("Running Saha end-to-end testbench with {} test cases (threading: {})", test_cases.len(), args.use_threading);

    let mut test_runner: TestRunner = TestRunner::new(test_cases, args.command, args.use_threading);

    test_runner.run();

    if !test_runner.success {
        test_runner.print_errors();
    }

    if args.allow_failure {
        return 0;
    }

    return test_runner.exit_status();
}