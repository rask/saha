//! Test case

use std::{
    collections::HashMap,
    path::PathBuf
};

use regex;

/// A single test unit to run with the test bench.
#[derive(Clone, Debug)]
pub struct TestCase {
    pub file: PathBuf,
    pub saha_source: Option<String>,
    pub desired_output: Option<String>,
    pub desired_exit_code: Option<i32>,
    pub actual_output: Option<String>,
    pub actual_exit_code: Option<i32>,
    pub success: bool
}

/// A collection of test cases.
pub struct TestCaseCollection {
    pub test_cases: Vec<TestCase>,
    pub failures: Vec<TestCase>,
    pub successes: Vec<TestCase>
}

impl TestCaseCollection {
    /// Create a new collection.
    pub fn new(test_cases: Vec<TestCase>) -> TestCaseCollection {
        return TestCaseCollection {
            test_cases: test_cases,
            failures: Vec::new(),
            successes: Vec::new()
        };
    }

    /// Get the length of the collection.
    pub fn len(&self) -> usize {
        return self.test_cases.len();
    }
}

/// Parse the Saha source code from the test case source.
fn get_test_case_source(source: &str) -> String {
    let match_ptrn = regex::Regex::new("(?s)^-----BEGIN SOURCE-----\n(.*?)\n-----BEGIN OUTPUT-----.*").unwrap();

    let matched = match_ptrn.captures(source).unwrap();

    return matched[1].to_string();
}

/// Parse the output part from the test case source.
fn get_test_case_desired_output(source: &str) -> String {
    let match_ptrn = regex::Regex::new("(?s)^.*-----BEGIN OUTPUT-----\n(.*?)\n-----BEGIN STATUS-----.*").unwrap();

    let matched = match_ptrn.captures(source).unwrap();

    return matched[1].to_string();
}

/// Parse the exit code from test case source.
fn get_test_case_desired_exit_code(source: &str) -> i32 {
    let match_ptrn = regex::Regex::new("(?s).*([0-9]+)\n?$").unwrap();

    let matched = match_ptrn.captures(source).unwrap();

    return matched[1].to_string().parse::<i32>().unwrap_or(-1024);
}

/// Wrap a source code portion inside a simple main function.
fn wrap_source_into_main_fn(source: String) -> String {
    return format!("function main() int\n{{\n{}\nreturn 0;\n}}", source);
}

/// Parse a collection of test case files into test cases.
pub fn parse_files_to_test_cases(files: HashMap<PathBuf, Result<String, ()>>) -> TestCaseCollection {
    let cases: Vec<TestCase> = files.into_iter().map(|(path, source)| {
        if source.is_err() {
            return TestCase {
                file: path,
                saha_source: None,
                desired_output: None,
                desired_exit_code: None,
                actual_output: None,
                actual_exit_code: None,
                success: false
            };
        }

        let source = source.ok().unwrap();

        let mut test_source = get_test_case_source(&source);
        let test_output = get_test_case_desired_output(&source);
        let test_exit = get_test_case_desired_exit_code(&source);

        if !test_source.contains("function main() int") {
            test_source = wrap_source_into_main_fn(test_source);
        }

        TestCase {
            file: path,
            saha_source: Some(test_source),
            desired_output: Some(test_output),
            desired_exit_code: Some(test_exit),
            actual_output: None,
            actual_exit_code: None,
            success: false
        }
    }).collect();

    return TestCaseCollection::new(cases);
}