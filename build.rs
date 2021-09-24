// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::{env::var, path::PathBuf, process::Command, str::from_utf8};

use bindgen::Builder;
use semver::Version;

static SUPPORTED_VERSIONS: &[&str] = &["4.37.0"];

fn main() {
    let bindings = Builder::default()
        .header("dm-ioctl.h")
        .derive_debug(true)
        .derive_default(true)
        .generate()
        .expect("Could not generate bindings");

    let out_path = var("OUT_DIR").unwrap();

    let bindings_path: PathBuf = [&out_path, "bindings.rs"].iter().collect();
    bindings
        .write_to_file(&bindings_path)
        .expect("Could not write bindings to file");

    let exec_path: PathBuf = [&out_path, "dm_version"].iter().collect();

    let rust_exec = var("RUSTC").unwrap_or_else(|_| "rustc".to_string());

    let mut rust_cmd = Command::new(rust_exec);
    let rust_cmd = rust_cmd
        .arg("--crate-type")
        .arg("bin")
        .arg("-o")
        .arg(&exec_path)
        .arg("dm_version.rs");

    let rustc_output = rust_cmd.output().expect("invoking rustc cmd failed");

    assert!(
        rustc_output.status.success(),
        "{}",
        from_utf8(&rustc_output.stderr).unwrap()
    );

    let version_output = Command::new(exec_path)
        .output()
        .expect("invoking version command failed");

    let version_output_stdout =
        from_utf8(&version_output.stdout).expect("dm_version executable failed to output utf8");

    let version = Version::parse(version_output_stdout)
        .expect("dm_version executable's version is unparseable");

    for ver in SUPPORTED_VERSIONS.iter().take_while(|ver_string| {
        let iter_version = Version::parse(ver_string).expect("Could not parse version");
        version >= iter_version
    }) {
        println!(
            "cargo:rustc-cfg=devicemapper{}supported",
            ver.split('.').take(2).collect::<Vec<_>>().join("")
        );
    }
}
