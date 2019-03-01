// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::borrow::Borrow;
use std::fmt;
use std::ops::Deref;

use crate::result::{DmError, DmResult};

use crate::core::deviceinfo::{DM_NAME_LEN, DM_UUID_LEN};
use crate::core::errors::ErrorKind;

/// An error function to construct an error when creating a new string id.
fn err_func(err_msg: &str) -> DmError {
    DmError::Core(ErrorKind::InvalidArgument(err_msg.into()).into())
}

/// A devicemapper name. Really just a string, but also the argument type of
/// DevId::Name. Used in function arguments to indicate that the function
/// takes only a name, not a devicemapper uuid.
str_id!(DmName, DmNameBuf, DM_NAME_LEN, err_func);

/// A devicemapper uuid. A devicemapper uuid has a devicemapper-specific
/// format.
str_id!(DmUuid, DmUuidBuf, DM_UUID_LEN, err_func);

/// Used as a parameter for functions that take either a Device name
/// or a Device UUID.
#[derive(Debug, PartialEq, Eq)]
pub enum DevId<'a> {
    /// The parameter is the device's name
    Name(&'a DmName),
    /// The parameter is the device's devicemapper uuid
    Uuid(&'a DmUuid),
}

impl<'a> fmt::Display for DevId<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DevId::Name(name) => write!(f, "{}", name),
            DevId::Uuid(uuid) => write!(f, "{}", uuid),
        }
    }
}
