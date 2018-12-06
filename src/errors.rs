// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std;
use std::fmt;
use std::path::PathBuf;

use failure::{Backtrace, Context, Fail};
use nix;

use super::deviceinfo::DeviceInfo;

#[derive(Debug, Fail)]
pub enum ErrorKind {
    /// An error returned on failure to create a devicemapper context.
    ContextInitError { e: std::io::Error },
    /// This is a generic error that can be returned when a method
    /// receives an invalid argument. Ideally, the argument should be
    /// invalid in itself, i.e., it should not be made invalid by some
    /// part of the program state or the environment.
    InvalidArgument { t: String },
    /// An error returned exclusively by DM methods.
    /// This error is initiated in DM::do_ioctl and returned by
    /// numerous wrapper methods.
    IoctlError { t: Box<DeviceInfo>, n: nix::Error },
    /// An error returned when the response exceeds the maximum possible
    /// size of the ioctl buffer.
    IoctlResultTooLargeError,
    /// An error returned on failure to get metadata for a device
    MetadataIoError { path: PathBuf, e: std::io::Error },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::ContextInitError { ref e } => {
                write!(f, "DM context not initialized due to IO error: {}", e)
            }
            ErrorKind::InvalidArgument { ref t } => write!(f, "invalid argument: '{}'", t),
            ErrorKind::IoctlError { ref n, .. } => {
                write!(f, "low-level ioctl error due to nix error: {}", n)
            }
            ErrorKind::IoctlResultTooLargeError => write!(
                f,
                "ioctl result too large for maximum buffer size: 4294967295 bytes"
            ),
            ErrorKind::MetadataIoError { ref path, ref e } => write!(
                f,
                "failed to stat metadata for device at {} due to IO error: {}",
                path.to_string_lossy(),
                e
            ),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

impl Fail for Error {
    // TODO: This method will be deprecated in 1.33.0.
    // Replace it with source at that time.
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        self.inner.get_context()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            inner: Context::new(kind),
        }
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }
}

impl Error {
    // TODO: This method has been soft-deprecated as of 1.30.
    // Remove it as soon as lowest required version of rustc makes that
    // acceptable.
    pub fn description(&self) -> &str {
        match *self.kind() {
            ErrorKind::ContextInitError { .. } => "DM context not initialized due to IO error",
            ErrorKind::InvalidArgument { .. } => "invalid argument",
            ErrorKind::IoctlError { .. } => "low-level ioctl error due to nix error: {}",
            ErrorKind::IoctlResultTooLargeError => {
                "ioctl result too large for maximum buffer size: 4294967295 bytes"
            }
            ErrorKind::MetadataIoError { .. } => "failed to get metadata for a device",
        }
    }
}

#[cfg(test)]
mod tests {

    use failure::Fail;

    use super::{Error, ErrorKind};

    // A function that returns an error
    fn a_func() -> Result<(), Error> {
        Err(ErrorKind::IoctlResultTooLargeError.into())
    }

    #[test]
    /// Verify that backtrace is available
    fn test_backtrace() {
        let error = a_func().unwrap_err();
        assert!(error.backtrace().is_some());
    }
}
