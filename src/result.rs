// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::error::Error;
use std::fmt;
use std::io;
use nix;

///
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

/// Define a common error enum.
/// See http://blog.burntsushi.net/rust-error-handling/
#[derive(Debug)]
pub enum DmError {
    /// DM errors
    Dm(ErrorEnum, String),
    /// IO errors
    Io(io::Error),
    /// *nix Errors
    Nix(nix::Error),
}

/// return result for DM functions
pub type DmResult<T> = Result<T, DmError>;

impl From<io::Error> for DmError {
    fn from(err: io::Error) -> DmError {
        DmError::Io(err)
    }
}

impl From<nix::Error> for DmError {
    fn from(err: nix::Error) -> DmError {
        DmError::Nix(err)
    }
}

impl fmt::Display for DmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DmError::Dm(ref err, ref msg) => write!(f, "DM error: {}: {}", err, msg),
            DmError::Io(ref err) => write!(f, "IO error: {}", err),
            DmError::Nix(ref err) => write!(f, "Nix error: {}", err.description()),
        }
    }
}


impl Error for DmError {
    fn description(&self) -> &str {
        match *self {
            DmError::Dm(_, ref msg) => msg,
            DmError::Io(ref err) => err.description(),
            DmError::Nix(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            DmError::Dm(_, _) => None,
            DmError::Io(ref err) => Some(err),
            DmError::Nix(ref err) => Some(err),
        }
    }
}
