// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
