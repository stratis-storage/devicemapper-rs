// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::{self, Debug};

use crate::core::util::str_from_c_str;

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[allow(non_snake_case)]
#[allow(clippy::redundant_static_lifetimes)]
mod bindings {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub use bindings::{
    dm_ioctl as Struct_dm_ioctl, dm_name_list as Struct_dm_name_list,
    dm_target_deps as Struct_dm_target_deps, dm_target_msg as Struct_dm_target_msg,
    dm_target_spec as Struct_dm_target_spec, dm_target_versions as Struct_dm_target_versions, *,
};

/// TODO: When https://github.com/rust-lang/rust-bindgen/issues/2041 is resolved,
/// this implementation can be removed.
impl Debug for dm_ioctl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Struct_dm_ioctl")
            .field("version", &self.version)
            .field("data_size", &self.data_size)
            .field("data_start", &self.data_start)
            .field("target_count", &self.target_count)
            .field("open_count", &self.open_count)
            .field("flags", &self.flags)
            .field("event_nr", &self.event_nr)
            .field("padding", &self.padding)
            .field("dev", &self.dev)
            .field(
                "name",
                &str_from_c_str(&self.name as &[libc::c_char]).unwrap_or("Could not parse string"),
            )
            .field(
                "uuid",
                &str_from_c_str(&self.uuid as &[libc::c_char]).unwrap_or("Could not parse string"),
            )
            .field(
                "data",
                &str_from_c_str(&self.data as &[libc::c_char]).unwrap_or("Could not parse string"),
            )
            .finish()
    }
}
