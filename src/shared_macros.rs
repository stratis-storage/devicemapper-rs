// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

/// A module to contain functionality shared among the various types of
/// devices and implemented by means of macros.

macro_rules! name {
    ($s: ident) => {
        $s.dev_info.name()
    }
}

macro_rules! dstr {
    ($s: ident) => {
        $s.dev_info.device().to_string()
    }
}

macro_rules! devnode {
    ($s: ident) => {
        ["/dev", &format!("dm-{}", $s.dev_info.device().minor)].iter().collect()
    }
}
