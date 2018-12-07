//! Test runner

use std::{
    path::PathBuf,
    io::Write,
    process::{exit, Stdio, ChildStdin, Command}
};

use crate::testcase::{
    TestCase,
    TestCaseCollection
};

/// TestRunner takes in a collection of test cases and runs the required tests
/// against them.
pub struct TestRunner {
    test_cases: TestCaseCollection,
    binary: BinRunner,
    pub success: bool,
    failures: Vec<TestResult>
}

impl TestRunner {
    /// Test the binary exists and is operable.
    pub fn test_binary(binary: PathBuf) -> PathBuf {
        if binary.exists() {
            return binary;
        }

        eprintln!("Cannot test inexistent Saha binary");

        exit(1);
    }

    /// Create a new TestRunner.
    pub fn new(test_cases: TestCaseCollection, binary: PathBuf) -> TestRunner {
        let binary = TestRunner::test_binary(binary);

        let bin_runner = BinRunner {
            binary: binary
        };

        return TestRunner {
            test_cases: test_cases,
            binary: bin_runner,
            failures: Vec::new(),
            success: false,
        };
    }

    /// Run the tests.
    pub fn run(&mut self) {
        println!();

        let cases = self.test_cases.test_cases.clone();

        let mut current_line = 0;
        let tests_per_line = 32;
        let mut tests_on_current_line = 0;

        let mut fails = Vec::new();

        for c in cases {
            let result = self.binary.run(c);

            if result.success {
                print!(".");
            } else {
                fails.push(result);
                print!("F");
            }

            tests_on_current_line += 1;

            if tests_on_current_line >= tests_per_line {
                println!();
                current_line = current_line + 1;
                tests_on_current_line = 0;
            }
        }

        println!();

        if fails.is_empty() {
            self.success = true;
        } else {
            self.failures = fails;
            self.success = false;
        }
    }

    /// Get the test run status as an exit status code.
    pub fn exit_status(&self) -> i32 {
        if self.success {
            return 0;
        } else {
            return 1;
        }
    }

    /// Print failed tests for the user to peruse.
    pub fn print_errors(&self) {
        for e in &self.failures {
            let case = e.test_case.clone();
            let test_file = case.file.clone();

            println!();
            println!("Test case {:?} failed", test_file);
            println!();
            println!("Expected exit code:");
            println!();
            println!("{}", case.desired_exit_code.unwrap());
            println!();
            println!("Actual exit code:");
            println!();
            println!("{}", e.actual_exit);
            println!();
            println!("Expected output:");
            println!();
            println!("{}", case.desired_output.unwrap());
            println!();
            println!("Actual output:");
            println!();
            println!("{}", e.actual_output);
            println!();
            println!("-----");
        }
    }
}

/// Test result of a BinRunner command.
#[derive(Debug)]
pub struct TestResult {
    pub success: bool,
    pub test_case: TestCase,
    pub actual_output: String,
    pub actual_exit: i32
}

/// Binary runner.
struct BinRunner {
    binary: PathBuf
}

impl BinRunner {
    pub fn run(&self, test_case: TestCase) -> TestResult {
        let mut cmd = Command::new(&self.binary)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to execute test command");

        let stdin_result = cmd.stdin.as_mut().unwrap().write_all(test_case.saha_source.clone().unwrap().as_bytes());

        if stdin_result.is_err() {
            return TestResult {
                test_case: test_case.clone(),
                success: false,
                actual_output: "".to_string(),
                actual_exit: -1024
            };
        }

        let output = cmd.wait_with_output().unwrap();

        let actual_exit: i32 = output.status.code().unwrap();
        let mut actual_output: String = String::from_utf8_lossy(&output.stdout).to_string();

        if actual_output.len() == 0 && actual_exit > 0 {
            actual_output = String::from_utf8_lossy(&output.stderr).to_string();
        }

        let result = TestResult {
            test_case: test_case.clone(),
            success: test_case.desired_output.unwrap() == actual_output && test_case.desired_exit_code.unwrap() == actual_exit,
            actual_output: actual_output,
            actual_exit: actual_exit
        };

        return result;
    }
}