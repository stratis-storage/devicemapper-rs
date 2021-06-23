// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[cfg(target_os = "android")]
use super::util::str_from_byte_slice;
#[cfg(not(target_os = "android"))]
use crate::core::util::str_from_c_str;
use std::fmt::{self, Debug};

#[allow(non_camel_case_types)]
#[allow(dead_code)]
#[allow(non_snake_case)]
#[allow(clippy::redundant_static_lifetimes)]
#[allow(unknown_lints)]
#[allow(deref_nullptr)]
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
        #[cfg(not(target_os = "android"))]
        let name = str_from_c_str(&self.name as &[i8]);
        #[cfg(target_os = "android")]
        let name = str_from_byte_slice(&self.name);

        #[cfg(not(target_os = "android"))]
        let uuid = str_from_c_str(&self.uuid as &[i8]);
        #[cfg(target_os = "android")]
        let uuid = str_from_byte_slice(&self.uuid);

        #[cfg(not(target_os = "android"))]
        let data = str_from_c_str(&self.data as &[i8]);
        #[cfg(target_os = "android")]
        let data = str_from_byte_slice(&self.data);

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
            .field("name", &name.unwrap_or("Could not parse string"))
            .field("uuid", &uuid.unwrap_or("Could not parse string"))
            .field("data", &data.unwrap_or("Could not parse string"))
            .finish()
    }
}
