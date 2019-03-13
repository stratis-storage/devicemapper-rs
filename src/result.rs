// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::error::Error;
use std::fmt;
use std::io;

use crate::core::errors;

/// A very simple breakdown of outer layer errors.
#[derive(Debug)]
pub enum ErrorEnum {
    /// generic error code
    Error,
    /// invalid value passed as argument
    Invalid,
    /// something not found
    NotFound,
}

impl fmt::Display for ErrorEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// Super error type, with constructors distinguishing outer errors from
/// core errors.
#[derive(Debug)]
pub enum DmError {
    /// DM errors
    Dm(ErrorEnum, String),
    /// IO errors
    Io(io::Error),
    /// Errors in the core devicemapper functionality
    Core(errors::Error),
}

/// return result for DM functions
pub type DmResult<T> = Result<T, DmError>;

impl From<errors::Error> for DmError {
    fn from(err: errors::Error) -> DmError {
        DmError::Core(err)
    }
}

impl From<io::Error> for DmError {
    fn from(err: io::Error) -> DmError {
        DmError::Io(err)
    }
}

impl fmt::Display for DmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DmError::Dm(ref err, ref msg) => write!(f, "DM error: {}: {}", err, msg),
            DmError::Io(ref err) => write!(f, "Io error: {}", err),
            DmError::Core(ref err) => write!(f, "DM Core error: {}", err),
        }
    }
}

impl Error for DmError {
    fn description(&self) -> &str {
        match *self {
            DmError::Dm(_, ref msg) => msg,
            DmError::Io(ref err) => err.description(),
            DmError::Core(ref err) => err.description(),
        }
    }
}
