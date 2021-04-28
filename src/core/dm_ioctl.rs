// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{
    ffi::CString,
    fmt::{self, Debug},
};

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
        fn c_char_slice_to_string(slice: &[libc::c_char]) -> Option<String> {
            let cstring = CString::new(
                slice
                    .iter()
                    .map(|&c| c as u8)
                    .take_while(|c| *c != 0u8)
                    .chain([0].iter().cloned())
                    .collect::<Vec<_>>(),
            )
            .ok()?;
            cstring.into_string().ok()
        }

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
                &c_char_slice_to_string(&self.name as &[libc::c_char])
                    .unwrap_or_else(|| "Could not parse string".to_string()),
            )
            .field(
                "uuid",
                &c_char_slice_to_string(&self.uuid as &[libc::c_char])
                    .unwrap_or_else(|| "Could not parse string".to_string()),
            )
            .field(
                "data",
                &c_char_slice_to_string(&self.data as &[libc::c_char])
                    .unwrap_or_else(|| "Could not parse string".to_string()),
            )
            .finish()
    }
}
