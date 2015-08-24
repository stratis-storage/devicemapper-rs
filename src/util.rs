// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

pub fn align_to(num: usize, align_to: usize) -> usize {
    let agn = align_to - 1;

    (num + agn) & !agn
}
