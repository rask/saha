use std::path::PathBuf;
use structopt::StructOpt;

/// Saha Testbench
#[derive(StructOpt, Debug)]
#[structopt(name = "saha_testbench")]
pub struct CliArgs {
    /// The Saha interpreter binary to test.
    #[structopt(name = "BINARY", parse(from_os_str))]
    pub binary: PathBuf,

    /// Test case file, or a directory containing test case files.
    #[structopt(name = "FILE", parse(from_os_str))]
    pub test: PathBuf,

    #[structopt(long = "allow-failure")]
    pub allow_failure: bool,
}

/// Get command line arguments given to the interpreter.
pub fn get_cli_arguments() -> CliArgs {
    return CliArgs::from_args();
}