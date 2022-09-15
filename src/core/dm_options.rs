// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use crate::{
    core::dm_flags::{DmCookie, DmFlags},
    Device,
};

/// Encapsulates options for device mapper calls
#[derive(Clone, Copy, Debug)]
pub struct DmOptions {
    flags: DmFlags,
    cookie: DmCookie,
    device: Device,
}

impl DmOptions {
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

    /// Sets the `Device` value for self. Replaces the previous value.
    /// Consumes self.
    pub fn set_device(mut self, device: Device) -> DmOptions {
        self.device = device;
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

    pub fn device(&self) -> Device {
        self.device
    }
}

impl Default for DmOptions {
    fn default() -> Self {
        Self {
            flags: Default::default(),
            cookie: Default::default(),
            device: Device { major: 0, minor: 0 },
        }
    }
}
