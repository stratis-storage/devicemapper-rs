// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{self, path::PathBuf};

use crate::core::deviceinfo::DeviceInfo;

error_chain! {
    errors {
        /// An error returned on failure to create a devicemapper context.
        ContextInitError(e: std::io::Error) {
            description("DM context not initialized")
            display("DM context not initialized due to IO error: {}", e)
        }

        /// This is a generic error that can be returned when a method
        /// receives an invalid argument. Ideally, the argument should be
        /// invalid in itself, i.e., it should not be made invalid by some
        /// part of the program state or the environment.
        InvalidArgument(t: String) {
            description("an invalid argument was passed")
            display("invalid argument: '{}'", t)
        }

        /// An error returned exclusively by DM methods.
        /// This error is initiated in DM::do_ioctl and returned by
        /// numerous wrapper methods.
        IoctlError(t: Box<DeviceInfo>, n: nix::Error) {
            description("low-level ioctl error")
            display("low-level ioctl error due to nix error: {}", n)
        }

        /// An error returned when the response exceeds the maximum possible
        /// size of the ioctl buffer.
        IoctlResultTooLargeError {
            description("ioctl result too large for maximum buffer size: 4294967295 bytes")
            display("ioctl result too large for maximum buffer size: 4294967295 bytes")
        }

        /// An error returned on failure to get metadata for a device
        MetadataIoError(path: PathBuf, e: std::io::Error) {
            description("failed to get metadata for a device")
            display("failed to stat metadata for device at {} due to IO error: {}", path.to_string_lossy(), e)
        }

        /// An error returned on general IO failure
        GeneralIoError(e: std::io::Error) {
            description("failed to perform IO operation")
            display("failed to perform operation due to IO error: {}", e)
        }
    }
}
