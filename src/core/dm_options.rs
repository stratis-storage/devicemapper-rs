// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
use crate::core::{DmCookie, DmFlags};

/// Encapsulates options for device mapper calls
#[derive(Debug, Default, Clone)]
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

    /// Set the DmFlags value for option.  Note this call is not additive in that it sets (replaces)
    /// entire flag value in one call.  Thus if you want to incrementally add additional flags you
    /// need to retrieve current and '|' with new.
    ///
    /// ```no_run
    /// use devicemapper::DmFlags;
    /// use devicemapper::DmOptions;
    ///
    /// let mut options = DmOptions::new();
    /// options.set_flags(DmFlags::DM_READONLY);
    /// let flags = DmFlags::DM_PERSISTENT_DEV | options.flags();
    /// options.set_flags(flags);
    /// ```
    pub fn set_flags(&mut self, flags: DmFlags) -> &mut DmOptions {
        self.flags = flags;
        self
    }

    /// Set the cookie bitfield value for option (overloaded meaning with event_nr in header).
    /// Note this call is not additive in that it sets (replaces) entire cookie value in one call.
    /// Thus if you want to incrementally add additional flags you need to retrieve current and '|'
    /// with new.
    ///
    /// ```no_run
    /// use devicemapper::{DmCookie, DmFlags};
    /// use devicemapper::DmOptions;
    ///
    /// let mut options = DmOptions::new();
    /// options.set_cookie(DmCookie::DM_UDEV_PRIMARY_SOURCE_FLAG);
    ///
    /// let new_cookie = options.cookie() | DmCookie::DM_UDEV_DISABLE_DM_RULES_FLAG;
    /// options.set_cookie(new_cookie);
    /// ```
    pub fn set_cookie(&mut self, cookie: DmCookie) -> &mut DmOptions {
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
