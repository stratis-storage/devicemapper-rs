// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(dead_code)]

error_chain! {
    errors {
        /// An error returned on failure to create a devicemapper context.
        ContextInitError {
            description("DM context not initialized")
            display("DM context not initialized")
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
        IoctlError {
            description("low-level ioctl error")
            display("low-level ioctl error")
        }
    }
}
