//! Imports
//!
//! Import/use related token data, e.g. import type and aliasing.

use std::path::PathBuf;

/// Import definition.
#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    /// Project level import. Contains member to import and the aliased name to use for it.
    /// Also the file path to import is included.
    Project(String, String, PathBuf),

    /// Vendor/dependency level import. Contains member to import and the aliased name to use for
    /// it. Also the file path to import is included.
    Vendor(String, String, PathBuf),

    /// Standard library level import. Contains member to import and the aliased name to use for it.
    Std(String, String),

    /// Extension level import. Contains member to import and the aliased name to use for it.
    Ext(String, String)
}