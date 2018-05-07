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

    /// Set the DmFlags value for option
    pub fn set_flags(mut self, flags: DmFlags) -> DmOptions {
        self.flags = flags;
        self
    }

    /// Set the event_nr value for option
    pub fn set_event_nr(mut self, event_nr: u32) -> DmOptions {
        self.event_nr = event_nr;
        self
    }

    /// Retrieve the flags value
    pub fn get_flags(&self) -> DmFlags {
        self.flags
    }

    /// Retrieve the event_nr value
    pub fn get_event_nr(&self) -> u32 {
        self.event_nr
    }
}
