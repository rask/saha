//! Saha Testbench Binary
//!
//! Runs naive end-to-end tests against a compiled Saha interpreter binary.

use std::process::exit;
use saha_testbench::run;

fn main() {
    exit(run());
}