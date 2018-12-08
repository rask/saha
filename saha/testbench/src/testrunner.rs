//! Test runner

use std::{
    thread,
    sync::{Arc, Mutex},
    io::Write,
    process::{exit, Stdio, Command}
};

use crate::testcase::{
    TestCase,
    TestCaseCollection
};

/// TestRunner takes in a collection of test cases and runs the required tests
/// against them.
pub struct TestRunner {
    test_cases: TestCaseCollection,
    command: BinRunner,
    use_threads: bool,
    pub success: bool,
    failures: Vec<TestResult>
}

impl TestRunner {
    /// Test the binary exists and is operable.
    pub fn test_command(command: String) -> String {
        return command;
    }

    /// Create a new TestRunner.
    pub fn new(test_cases: TestCaseCollection, command: String, use_threads: bool) -> TestRunner {
        let command = TestRunner::test_command(command);

        let bin_runner = BinRunner {
            command: command
        };

        return TestRunner {
            test_cases: test_cases,
            command: bin_runner,
            use_threads: use_threads,
            failures: Vec::new(),
            success: false,
        };
    }

    /// Run the tests.
    ///
    /// FIXME: does the threading work as it should?
    pub fn run(&mut self) {
        println!();

        if !self.use_threads {
            self.run_single_threaded();

            return;
        }

        let cases = self.test_cases.test_cases.clone();

        let current_line = Arc::new(Mutex::new(0));
        let tests_on_current_line = Arc::new(Mutex::new(0));
        let tests_per_line = 32;

        let mut workers: Vec<thread::JoinHandle<_>> = Vec::new();

        for c in cases {
            let cmd = self.command.clone();
            let ref_cl = Arc::clone(&current_line);
            let ref_tocl = Arc::clone(&tests_on_current_line);

            workers.push(thread::spawn(move || {
                let res = cmd.run(&c);

                if res.success {
                    print!(".");
                } else {
                    print!("F");
                }

                {
                    let mut cl = ref_cl.lock().unwrap();
                    let mut tocl = ref_tocl.lock().unwrap();

                    *tocl += 1;

                    if *tocl >= tests_per_line {
                        println!();
                        *cl += 1;
                        *tocl = 0;
                    }
                }

                res
            }));
        }

        let mut workers_results: Vec<TestResult> = Vec::new();

        for w in workers {
            workers_results.push(w.join().ok().unwrap());
        }

        println!();

        let fails: Vec<_> = workers_results.iter().filter(|tr| {
            !tr.success
        }).map(|tr| tr.clone()).collect();

        if fails.is_empty() {
            self.success = true;
        } else {
            self.failures = fails;
            self.success = false;
        }
    }

    /// Run all the tests in sequence using a single thread.
    fn run_single_threaded(&mut self) {
        let cases = self.test_cases.test_cases.clone();
        let mut current_line = 0;
        let mut tests_on_current_line = 0;
        let tests_per_line = 32;
        let mut fails: Vec<_> = Vec::new();

        for c in cases {
            let res = self.command.run(&c);

            if res.success {
                print!(".");
            } else {
                fails.push(res);
                print!("F");
            }

            tests_on_current_line += 1;

            if tests_on_current_line >= tests_per_line {
                println!();
                current_line = current_line + 1;
                tests_on_current_line = 0;
            }
        }

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
#[derive(Debug, Clone)]
pub struct TestResult {
    pub success: bool,
    pub test_case: TestCase,
    pub actual_output: String,
    pub actual_exit: i32
}

/// Binary runner.
#[derive(Clone)]
struct BinRunner {
    command: String
}

impl BinRunner {
    pub fn run(&self, test_case: &TestCase) -> TestResult {
        let test_cmd = self.command.clone();

        let mut cmd_parts: Vec<&str> = test_cmd.split(' ').collect();

        let cmd_first = cmd_parts.remove(0);

        let mut cmd_src = Command::new(cmd_first);

        for p in cmd_parts {
            cmd_src.arg(p);
        }

        let mut cmd = cmd_src
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
            success: test_case.desired_output.clone().unwrap() == actual_output && test_case.desired_exit_code.clone().unwrap() == actual_exit,
            actual_output: actual_output,
            actual_exit: actual_exit
        };

        return result;
    }
}