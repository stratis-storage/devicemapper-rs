// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std;

use crate::core::DeviceInfo;

use backtrace::Backtrace;

#[derive(Debug)]
pub enum ErrorKind {
    /// An error returned on failure to create a devicemapper context.
    ContextInitError,

    /// This is a generic error that can be returned when a method
    /// receives an invalid argument. Ideally, the argument should be
    /// invalid in itself, i.e., it should not be made invalid by some
    /// part of the program state or the environment.
    InvalidArgument { description: String },

    /// An error returned exclusively by DM methods.
    /// This error is initiated in DM::do_ioctl and returned by
    /// numerous wrapper methods.
    IoctlError { device_info: Box<DeviceInfo> },

    /// An error returned when the response exceeds the maximum possible
    /// size of the ioctl buffer.
    IoctlResultTooLarge,

    /// An error returned on failure to get metadata for a device
    MetadataIoError { device_path: std::path::PathBuf },
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorKind::ContextInitError => write!(f, "DM context not initialized"),
            ErrorKind::InvalidArgument { description } => {
                write!(f, "invalid argument: {}", description)
            }
            // Do not print any device_info information, it's too low-level
            ErrorKind::IoctlError { .. } => write!(f, "low-level ioctl error"),
            ErrorKind::IoctlResultTooLarge => write!(
                f,
                "ioctl result too large for maximum buffer size 4294967295 bytes"
            ),
            ErrorKind::MetadataIoError { device_path } => write!(
                f,
                "failed to stat metadata for device at {}",
                device_path.to_string_lossy()
            ),
        }
    }
}

#[derive(Debug)]
/// What relation the component error has to its parent
enum Suberror {
    /// The error occurred before the parent error
    Previous(Box<dyn std::error::Error>),
    /// The error is further explained or extended by the parent
    Constituent(Box<dyn std::error::Error>),
}

#[derive(Debug)]
pub struct Error {
    // The source of the error, which may be an error for
    // which this error is a further explanation, i.e., a
    // constituent error, or it may simply be an error that occurred
    // previously, and which presumably caused the current code to
    // be run and encounter its own, novel error.
    source_impl: Option<Suberror>,

    // The backtrace at the site the error is returned
    backtrace: Backtrace,

    // Distinguish among different errors with an ErrorKind
    pub specifics: ErrorKind,
}

impl Error {
    fn new(kind: ErrorKind) -> Error {
        Error {
            backtrace: Backtrace::new(),
            source_impl: None,
            specifics: kind,
        }
    }

    /// Return the optional backtrace associated with this error.
    // Note that the function name is our_backtrace, so that it does not
    // conflict with a future possible backtrace function in the Error trait.
    pub fn our_backtrace(&self) -> Option<&Backtrace> {
        Some(&self.backtrace)
    }

    /// Set extension as the extension on this error.
    /// Return the head of the chain, now subsequent.
    pub fn set_extension(self, mut extension: Error) -> Error {
        extension.source_impl = Some(Suberror::Constituent(Box::new(self)));
        extension
    }

    /// Set subsequent as the subsequent error for this error.
    /// Return the head of the chain, now subsequent.
    pub fn set_subsequent(self, mut subsequent: Error) -> Error {
        subsequent.source_impl = Some(Suberror::Previous(Box::new(self)));
        subsequent
    }

    /// Set constituent as the constituent of this error.
    pub fn set_constituent(mut self, constituent: Box<dyn std::error::Error>) -> Error {
        self.source_impl = Some(Suberror::Constituent(constituent));
        self
    }

    /// Set previous as the previous error.
    pub fn set_previous(mut self, previous: Box<dyn std::error::Error>) -> Error {
        self.source_impl = Some(Suberror::Previous(previous));
        self
    }

    /// Obtain the immediate previous error, if there is one
    pub fn previous(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.source_impl.as_ref() {
            Some(Suberror::Previous(c)) => Some(&**c),
            _ => None,
        }
    }

    /// Obtain the immediate constituent error, if there is one
    pub fn constituent(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.source_impl.as_ref() {
            Some(Suberror::Constituent(c)) => Some(&**c),
            _ => None,
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error::new(kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source_impl.as_ref().map(|c| match c {
            Suberror::Previous(c) => &**c,
            Suberror::Constituent(c) => &**c,
        })
    }

    // deprecated in 1.33.0
    // identical to source()
    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source_impl.as_ref().map(|c| match c {
            Suberror::Previous(c) => &**c,
            Suberror::Constituent(c) => &**c,
        })
    }
}

// Display only the message associated w/ the specifics.
// Consider the rest to be management baggage.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.specifics)
    }
}
