// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{env::var, path::PathBuf};

use bindgen::{Builder, RustTarget};
use pkg_config::{Config, Library};

fn libdevmapper_probe() -> Library {
    match Config::new()
        .atleast_version("1.02.151")
        .cargo_metadata(false)
        .probe("devmapper")
    {
        Ok(library) => library,
        Err(e) => panic!("Suitable version of libdevmapper not found: {}", e),
    }
}

fn main() {
    let libdevmapper = libdevmapper_probe();
    // Generate bindings for dm-ioctl.h and libdevmapper.h
    // dm-ioctl.h is part of linux-headers/libc and has no pkg-config
    let bindings = Builder::default()
        .rust_target(RustTarget::Stable_1_73)
        .header("dm.h")
        .clang_args(
            libdevmapper
                .include_paths
                .iter()
                .map(|include| format!("-I{}", include.display())),
        )
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
        .expect("Could not generate dm.h bindings");

    let mut bindings_path = PathBuf::from(var("OUT_DIR").unwrap());
    bindings_path.push("dm-bindings.rs");
    bindings
        .write_to_file(&bindings_path)
        .expect("Could not write bindings to file");

    // Generate bindings for SysV Semaphore IPC
    let bindings = Builder::default()
        .rust_target(RustTarget::Stable_1_73)
        .header("sem.h")
        .default_macro_constant_type(bindgen::MacroTypeVariation::Signed)
        .allowlist_var("GET.*")
        .allowlist_var("SET.*")
        .allowlist_var("SEM_.*")
        .allowlist_type("semun");

    #[cfg(target_env = "musl")]
    let bindings = bindings
        .allowlist_type("seminfo")
        .allowlist_type("semid_ds");

    let bindings = bindings
        .derive_debug(true)
        .derive_default(true)
        .generate()
        .expect("Could not generate sem.h bindings");

    let mut bindings_path = PathBuf::from(var("OUT_DIR").unwrap());
    bindings_path.push("sem-bindings.rs");
    bindings
        .write_to_file(&bindings_path)
        .expect("Could not write bindings to file");
}
