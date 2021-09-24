// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.


mod bindings {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

fn main() {
    print!("{}.{}.{}", bindings::DM_VERSION_MAJOR, bindings::DM_VERSION_MINOR, bindings::DM_VERSION_PATCHLEVEL);
}
