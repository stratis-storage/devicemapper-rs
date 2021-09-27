#![allow(non_camel_case_types)]
#![allow(clippy::all)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));


// TODO: Remove Debug implementation and all its dependencies when able to
// upgrade to bindgen 0.59.
// https://github.com/stratis-storage/devicemapper-rs/issues/690

use std::{fmt::{self, Debug}, slice, str::from_utf8};

use nix::libc::c_char;

// Convert from a &[c_char] to a &[u8].
fn byte_slice_from_c_str(c_str: &[c_char]) -> &[u8] {
    unsafe { slice::from_raw_parts(c_str as *const _ as *const u8, c_str.len()) }
}

// Return a String parsed from the C string up to the first \0, or None
fn str_from_c_str(slc: &[c_char]) -> Option<&str> {
    let slc = byte_slice_from_c_str(slc);
    str_from_byte_slice(slc)
}

// Return a String parsed from the byte slice up to the first \0, or None
fn str_from_byte_slice(slc: &[u8]) -> Option<&str> {
    slc.iter()
        .position(|c| *c == b'\0')
        .and_then(|i| from_utf8(&slc[..i]).ok())
}

impl Debug for dm_ioctl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                &str_from_c_str(&self.name as &[nix::libc::c_char])
                    .unwrap_or("Could not parse string"),
            )
            .field(
                "uuid",
                &str_from_c_str(&self.uuid as &[nix::libc::c_char])
                    .unwrap_or("Could not parse string"),
            )
            .field(
                "data",
                &str_from_c_str(&self.data as &[nix::libc::c_char])
                    .unwrap_or("Could not parse string"),
            )
            .finish()
    }
}
