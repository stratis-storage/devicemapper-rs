// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::hash_set::HashSet;

/// The smallest number divisible by `align_to` and at least `num`.
/// Precondition: `align_to` is a power of 2.
/// Precondition: `num` + `align_to` < usize::MAX + 1.
pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}

/// Return slc up to the first \0, or None
pub fn slice_to_null(slc: &[u8]) -> Option<&[u8]> {
    slc.iter().position(|c| *c == b'\0').map(|i| &slc[..i])
}

/// Given feature arguments specified as HashSet, convert to String.
pub fn feature_args_to_string(features: &HashSet<String>) -> String {
    if features.is_empty() {
        "0".to_owned()
    } else {
        format!(
            "{} {}",
            features.len(),
            features.iter().cloned().collect::<Vec<_>>().join(" ")
        )
    }
}
