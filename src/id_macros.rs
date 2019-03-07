// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// A module to contain functionality used for generating DM ids which
// are restricted in length and format by devicemapper.

// Evaluates to an error string if the value does not match the requirements.
macro_rules! str_check {
    ($value:expr, $max_allowed_chars:expr) => {{
        let value = $value;
        let max_allowed_chars = $max_allowed_chars;
        if !value.is_ascii() {
            Some(format!("value {} has some non-ascii characters", value))
        } else {
            let num_chars = value.len();
            if num_chars == 0 {
                Some("value has zero characters".into())
            } else if num_chars > max_allowed_chars {
                Some(format!(
                    "value {} has {} chars which is greater than maximum allowed {}",
                    value, num_chars, max_allowed_chars
                ))
            } else {
                None
            }
        }
    }};
}

/// Define borrowed and owned versions of string types that guarantee
/// conformance to DM restrictions, such as maximum length.
// This implementation follows the example of Path/PathBuf as closely as
// possible.
macro_rules! str_id {
    ($B:ident, $O:ident, $MAX:ident, $err_func:ident) => {
        /// The borrowed version of the DM identifier.
        #[derive(Debug, PartialEq, Eq, Hash)]
        pub struct $B {
            inner: str,
        }

        /// The owned version of the DM identifier.
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $O {
            inner: String,
        }

        impl $B {
            /// Create a new borrowed identifier from a `&str`.
            #[allow(clippy::new_ret_no_self)]
            pub fn new(value: &str) -> DmResult<&$B> {
                if let Some(err_msg) = str_check!(value, $MAX - 1) {
                    return Err($err_func(&err_msg));
                }
                Ok(unsafe { &*(value as *const str as *const $B) })
            }

            /// Get the inner value as bytes
            pub fn as_bytes(&self) -> &[u8] {
                self.inner.as_bytes()
            }
        }

        impl ToOwned for $B {
            type Owned = $O;
            fn to_owned(&self) -> $O {
                $O {
                    inner: self.inner.to_owned(),
                }
            }
        }

        impl fmt::Display for $B {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", &self.inner)
            }
        }

        impl $O {
            /// Construct a new owned identifier.
            #[allow(clippy::new_ret_no_self)]
            pub fn new(value: String) -> DmResult<$O> {
                if let Some(err_msg) = str_check!(&value, $MAX - 1) {
                    return Err($err_func(&err_msg));
                }
                Ok($O { inner: value })
            }
        }

        impl AsRef<$B> for $O {
            fn as_ref(&self) -> &$B {
                self
            }
        }

        impl Borrow<$B> for $O {
            fn borrow(&self) -> &$B {
                self.deref()
            }
        }

        impl Deref for $O {
            type Target = $B;
            fn deref(&self) -> &$B {
                $B::new(&self.inner).expect("inner satisfies all correctness criteria for $B::new")
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;
    use std::fmt;
    use std::iter;
    use std::ops::Deref;

    use crate::result::{DmError, DmResult};

    use crate::core::errors::{Error, ErrorKind};

    fn err_func(err_msg: &str) -> DmError {
        DmError::Core(ErrorKind::InvalidArgument(err_msg.into()).into())
    }

    const TYPE_LEN: usize = 12;
    str_id!(Id, IdBuf, TYPE_LEN, err_func);

    #[test]
    /// Test for errors on an empty name.
    fn test_empty_name() {
        assert_matches!(Id::new(""),
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _)))) ;
        assert_matches!(IdBuf::new("".into()),
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _)))) ;
    }

    #[test]
    /// Test for errors on an overlong name.
    fn test_too_long_name() {
        let name = iter::repeat('a').take(TYPE_LEN + 1).collect::<String>();
        assert_matches!(Id::new(&name),
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _)))) ;
        assert_matches!(IdBuf::new(name),
            Err(DmError::Core(Error(ErrorKind::InvalidArgument(_), _)))) ;
    }

    #[test]
    /// Test the concrete methods and traits of the interface.
    fn test_interface() {
        let id = Id::new("id").expect("is valid id");
        let id_buf = IdBuf::new("id".into()).expect("is valid id");

        // Test as_bytes.
        assert_eq!(id.as_bytes(), &[105u8, 100u8]);
        assert_eq!(id_buf.as_bytes(), &[105u8, 100u8]);

        // Test ToOwned implementation.
        // $B.to_owned() == $O
        assert_eq!(id.to_owned(), id_buf);

        // Test Display implementation
        // X.to_string() = (*X).to_string()
        assert_eq!(id.to_string(), (*id).to_string());
        assert_eq!(id_buf.to_string(), (*id_buf).to_string());

        // Test Deref
        assert_eq!(id_buf.deref(), id);
        assert_eq!(*id_buf, *id);
    }
}
