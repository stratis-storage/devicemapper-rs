// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{env::var, path::PathBuf};

use bindgen::Builder;
use semver::Version;

static SPECIFIED_KERNEL_VERSIONS: &[&str] = &["4.13.0"];

fn main() {
    let version_str = var("MAXIMUM_KERNEL_VERSION").unwrap_or_else(|_| {
        SPECIFIED_KERNEL_VERSIONS
            .iter()
            .last()
            .expect("No specified versions")
            .to_string()
    });

    let version = Version::parse(&version_str).unwrap();

    for ver in SPECIFIED_KERNEL_VERSIONS.iter().take_while(|ver_string| {
        let iter_version = Version::parse(ver_string).expect("Could not parse version");
        version >= iter_version
    }) {
        println!(
            "cargo:rustc-cfg=devicemapper{}supported",
            ver.split('.').take(2).collect::<Vec<_>>().join("")
        );
    }

    let bindings = Builder::default()
        .header("dm-ioctl.h")
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
