use std::path::PathBuf;
use structopt::StructOpt;

/// Saha Testbench
#[derive(StructOpt, Debug)]
#[structopt(name = "saha_testbench")]
pub struct CliArgs {
    /// The Saha interpreter binary/command to test.
    #[structopt(long = "command")]
    pub command: String,

    /// Test case file, or a directory containing test case files.
    #[structopt(long = "test", parse(from_os_str))]
    pub test: PathBuf,

    #[structopt(long = "allow-failure")]
    pub allow_failure: bool,

    /// Whether to use threading to make the tests run faster. Disable when
    /// using kcov and similar coverage reporting tools.
    #[structopt(long = "use-threading")]
    pub use_threading: bool
}

/// Get command line arguments given to the interpreter.
pub fn get_cli_arguments() -> CliArgs {
    return CliArgs::from_args();
}