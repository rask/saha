//! Source
//! 
//! Anything related to managing source code, e.g. token/lexeme positioning and
//! code file locations.

use std::path::PathBuf;

/// Marks a specific position inside a source code file.
#[derive(Clone)]
pub struct FilePosition {
    pub path: PathBuf,
    pub line: isize,
    pub column: u32
}

impl FilePosition {
    /// Helper method for generating so called unknown file positions.
    /// 
    /// This is used when we want to specify a file position for logic which has
    /// no file or file position. Often these are internal language errors that
    /// do not stem from userland errors.
    pub fn unknown() -> FilePosition {
        return FilePosition {
            path: PathBuf::from("/unknown"),
            line: 0,
            column: 0
        };
    }
}