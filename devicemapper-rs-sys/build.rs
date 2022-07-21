// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{env::var, path::PathBuf};

use bindgen::Builder;

fn main() {
    let bindings = Builder::default()
        .header("dm.h")
        .allowlist_var("DM.*")
        .allowlist_type("__u16")
        .allowlist_type("dm_ioctl")
        .allowlist_type("dm_name_list")
        .allowlist_type("dm_target_deps")
        .allowlist_type("dm_target_msg")
        .allowlist_type("dm_target_spec")
        .allowlist_type("dm_target_versions")
        .derive_debug(true)
        .derive_default(true)
        .generate()
        .expect("Could not generate bindings");

    let mut bindings_path = PathBuf::from(var("OUT_DIR").unwrap());
    bindings_path.push("bindings.rs");
    bindings
        .write_to_file(&bindings_path)
        .expect("Could not write bindings to file");
}
