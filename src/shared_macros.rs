// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// A module to contain functionality shared among the various types of
// devices and implemented by means of macros.

macro_rules! device {
    ($s: ident) => {
        $s.dev_info.device()
    }
}

macro_rules! name {
    ($s: ident) => {
        $s.dev_info.name()
    }
}

macro_rules! uuid {
    ($s: ident) => {
        $s.dev_info.uuid()
    }
}

macro_rules! devnode {
    ($s: ident) => {
        ["/dev", &format!("dm-{}", $s.dev_info.device().minor)].iter().collect()
    }
}

macro_rules! to_raw_table_unique {
    ($s: ident) => {
        vec![($s.table.start,
              $s.table.length,
              $s.table.params.target_type(),
              $s.table.params.param_str())]
    }
}

macro_rules! table {
    ($s: ident) => {
        &$s.table
    }
}


/// All things we create in the unit test will have this utilized so we know what to cleanup.
/// If we change this we need to change the "tn" macro below too.
#[cfg(test)]
pub static DM_TEST_ID: &'static str = "_dm-rs_ut_delme";

/// Generate a test name (tn) which can be identified later, so that is can be cleaned up.
#[cfg(test)]
macro_rules! tn {
    ($name: expr) => {
        concat!($name, "_dm-rs_ut_delme")
    }
}
