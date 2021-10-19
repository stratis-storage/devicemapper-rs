// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use crate::core::dm_flags::{DmCookie, DmFlags};

/// Encapsulates options for device mapper calls
#[derive(Clone, Copy, Debug, Default)]
pub struct DmOptions {
    flags: DmFlags,
    cookie: DmCookie,
}

impl DmOptions {
    /// Create a new empty option
    pub fn new() -> DmOptions {
        DmOptions {
            flags: DmFlags::empty(),
            cookie: DmCookie::empty(),
        }
    }

    /// Set the DmFlags value for self. Replace the previous value.
    /// Consumes self.
    pub fn set_flags(mut self, flags: DmFlags) -> DmOptions {
        self.flags = flags;
        self
    }

    /// Set the DmCookie value for self. Replace the previous value.
    /// Consumes self.
    pub fn set_cookie(mut self, cookie: DmCookie) -> DmOptions {
        self.cookie = cookie;
        self
    }

    /// Retrieve the flags value
    pub fn flags(&self) -> DmFlags {
        self.flags
    }

    /// Retrieve the cookie value (used for input in upper 16 bits of event_nr header field).
    pub fn cookie(&self) -> DmCookie {
        self.cookie
    }
}
