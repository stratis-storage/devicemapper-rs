// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::io;
use nix;

///
#[derive(Debug)]
pub struct InternalError(pub Cow<'static, str>);

///
#[derive(Debug, Clone)]
pub enum ErrorEnum {
    /// generic error code
    Error,
    /// returned for failure to open a File
    FailedToOpen,
}

/// Define a common error enum.
/// See http://blog.burntsushi.net/rust-error-handling/
#[derive(Debug)]
pub enum DmError {
    /// DM errors
    Dm(InternalError),
    /// IO errors
    Io(io::Error),
    /// *nix Errors
    Nix(nix::Error),
}

/// return result for DM functions
pub type DmResult<T> = Result<T, DmError>;

impl From<InternalError> for DmError {
    fn from(err: InternalError) -> DmError {
        DmError::Dm(err)
    }
}

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

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl Error for InternalError {
    fn description(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for DmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DmError::Dm(ref err) => write!(f, "Stratis error: {}", err.0),
            DmError::Io(ref err) => write!(f, "IO error: {}", err),
            DmError::Nix(ref err) => write!(f, "Nix error: {}", err.errno().desc()),
        }
    }
}


impl Error for DmError {
    fn description(&self) -> &str {
        match *self {
            DmError::Dm(ref err) => &err.0,
            DmError::Io(ref err) => err.description(),
            DmError::Nix(ref err) => err.errno().desc(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            DmError::Dm(ref err) => Some(err),
            DmError::Io(ref err) => Some(err),
            DmError::Nix(ref err) => Some(err),
        }
    }
}
