// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use super::dm_flags::DmFlags;

/// Encapsulates options for device mapper calls
#[derive(Debug, Clone)]
pub struct DmOptions {
    flags: DmFlags,
    event_nr: u32,
}

impl DmOptions {
    /// Create a new empty option
    pub fn new() -> DmOptions {
        DmOptions {
            flags: DmFlags::empty(),
            event_nr: 0,
        }
    }

    /// Synonym for new()
    pub fn empty() -> DmOptions {
        DmOptions::new()
    }

    /// Set the DmFlags value for option.  Note this call is not additive in that it sets (replaces)
    /// entire flag value in one call.  Thus if you want to incrementally add additional flags you
    /// need to retrieve current and '|' with new.
    ///
    /// ```no_run
    /// use DmOptions;
    ///
    /// let mut options = DmOptions::new();
    /// options = options.set_flags(DmFlags::DM_READONLY);
    /// options = options
    ///               .set_flags(DmFlags::DM_PERSISTENT_DEV | options.flags());
    pub fn set_flags(mut self, flags: DmFlags) -> DmOptions {
        self.flags = flags;
        self
    }

    /// Set the cookie value for option (overloaded meaning with event_nr in header).
    pub fn set_cookie(mut self, value: u32) -> DmOptions {
        self.event_nr = value;
        self
    }

    /// Retrieve the flags value
    pub fn flags(&self) -> DmFlags {
        self.flags
    }

    /// Retrieve the event_nr value
    pub fn event_nr(&self) -> u32 {
        self.event_nr
    }
}
