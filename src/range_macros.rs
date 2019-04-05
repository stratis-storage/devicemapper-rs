// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// An omnibus macro that includes all simple macros.
macro_rules! range {
    ($T: ident, $display_name: expr) => {
        #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
        /// A type for $T
        pub struct $T(pub u64);
        checked_add!($T);
        debug_macro!($T);
        display!($T, $display_name);
        serde!($T);
        sum!($T);
        add!($T);
        add_assign!($T);
        sub!($T);
        sub_assign!($T);
        mul!($T);
        div!($T);
        rem!($T);
        deref!($T);
        from!($T);
    };
}

macro_rules! self_div {
    ($T:ident) => {
        impl Div<$T> for $T {
            type Output = u64;
            fn div(self, rhs: $T) -> u64 {
                self.0 / *rhs
            }
        }
    };
}

macro_rules! add {
    ($T:ident) => {
        impl Add<$T> for $T {
            type Output = $T;
            fn add(self, rhs: $T) -> $T {
                $T(self.0 + *rhs)
            }
        }
    };
}

macro_rules! sub {
    ($T:ident) => {
        impl Sub<$T> for $T {
            type Output = $T;
            fn sub(self, rhs: $T) -> $T {
                $T(self.0 - *rhs)
            }
        }
    };
}

macro_rules! add_assign {
    ($T:ident) => {
        impl AddAssign<$T> for $T {
            fn add_assign(&mut self, rhs: $T) {
                *self = $T(self.0 + *rhs)
            }
        }
    };
}

macro_rules! sub_assign {
    ($T:ident) => {
        impl SubAssign<$T> for $T {
            fn sub_assign(&mut self, rhs: $T) {
                *self = $T(self.0 - *rhs)
            }
        }
    };
}

macro_rules! deref {
    ($T:ident) => {
        impl Deref for $T {
            type Target = u64;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

macro_rules! from {
    ($T:ident) => {
        impl From<u64> for $T {
            fn from(t: u64) -> $T {
                $T(t)
            }
        }
    };
}

macro_rules! debug_macro {
    ($T:ident) => {
        impl fmt::Debug for $T {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, stringify!($T))?;
                write!(f, "({})", **self)
            }
        }
    };
}

macro_rules! display {
    ($T:ident, $display_name:expr) => {
        impl fmt::Display for $T {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{} {}", self.0, $display_name)
            }
        }
    };
}

macro_rules! serde {
    ($T:ident) => {
        impl serde::Serialize for $T {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                serializer.serialize_u64(**self)
            }
        }

        impl<'de> serde::Deserialize<'de> for $T {
            fn deserialize<D>(deserializer: D) -> Result<$T, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                Ok($T(serde::Deserialize::deserialize(deserializer)?))
            }
        }
    };
}

macro_rules! sum {
    ($T:ident) => {
        impl Sum for $T {
            fn sum<I: Iterator<Item = $T>>(iter: I) -> $T {
                iter.fold($T::default(), Add::add)
            }
        }
    };
}

// Define a complete set of division operations.
macro_rules! div {
    ($T: ident) => {
        unsigned_div!(u64, $T);
        unsigned_div!(u32, $T);
        unsigned_div!(u16, $T);
        unsigned_div!(u8, $T);
        usize_div!($T);
        self_div!($T);
    };
}

macro_rules! unsigned_div {
    ($t:ty, $T:ident) => {
        impl Div<$t> for $T {
            type Output = $T;
            fn div(self, rhs: $t) -> $T {
                $T(self.0 / u64::from(rhs))
            }
        }
    };
}

macro_rules! usize_div {
    ($T:ident) => {
        impl Div<usize> for $T {
            type Output = $T;
            fn div(self, rhs: usize) -> $T {
                #[allow(clippy::cast_lossless)]
                $T(self.0 / rhs as u64)
            }
        }
    };
}

// Define a complete set of multiplication operations.
macro_rules! mul {
    ($T: ident) => {
        unsigned_mul!(u64, $T);
        unsigned_mul!(u32, $T);
        unsigned_mul!(u16, $T);
        unsigned_mul!(u8, $T);
        usize_mul!($T);
    };
}

macro_rules! unsigned_mul {
    ($t:ty, $T:ident) => {
        impl Mul<$t> for $T {
            type Output = $T;
            fn mul(self, rhs: $t) -> $T {
                $T(self.0 * u64::from(rhs))
            }
        }

        impl Mul<$T> for $t {
            type Output = $T;
            fn mul(self, rhs: $T) -> $T {
                $T(u64::from(self) * rhs.0)
            }
        }
    };
}

macro_rules! usize_mul {
    ($T:ident) => {
        impl Mul<usize> for $T {
            type Output = $T;
            fn mul(self, rhs: usize) -> $T {
                #[allow(clippy::cast_lossless)]
                $T(self.0 * rhs as u64)
            }
        }

        impl Mul<$T> for usize {
            type Output = $T;
            fn mul(self, rhs: $T) -> $T {
                #[allow(clippy::cast_lossless)]
                $T(self as u64 * rhs.0)
            }
        }
    };
}

// Define a complete set of remainder operations.
macro_rules! rem {
    ($T: ident) => {
        unsigned_rem!(u64, $T);
        unsigned_rem!(u32, $T);
        unsigned_rem!(u16, $T);
        unsigned_rem!(u8, $T);
        usize_rem!($T);
        self_rem!($T);
    };
}

macro_rules! unsigned_rem {
    ($t:ty, $T:ident) => {
        impl Rem<$t> for $T {
            type Output = $T;
            fn rem(self, rhs: $t) -> $T {
                $T(self.0 % u64::from(rhs))
            }
        }
    };
}

macro_rules! usize_rem {
    ($T:ident) => {
        impl Rem<usize> for $T {
            type Output = $T;
            fn rem(self, rhs: usize) -> $T {
                #[allow(clippy::cast_lossless)]
                $T(self.0 % rhs as u64)
            }
        }
    };
}

macro_rules! self_rem {
    ($T:ident) => {
        impl Rem<$T> for $T {
            type Output = $T;
            fn rem(self, rhs: $T) -> $T {
                $T(self.0 % u64::from(rhs.0))
            }
        }
    };
}

macro_rules! checked_add {
    ($T: ident) => {
        impl $T {
            /// Add two items of type $T, return None if overflow.
            pub fn checked_add(&self, other: $T) -> Option<$T> {
                self.0.checked_add(other.0).map($T)
            }
        }
    };
}

#[cfg(test)]
mod tests {

    use std::fmt;
    use std::iter::Sum;
    use std::ops::{Add, AddAssign, Deref, Div, Mul, Rem, Sub, SubAssign};
    use std::u64;

    range!(Units, "units");

    #[test]
    /// Test implicit derivations for Units
    fn test_implicit_derivations() {
        // Test Clone. Note cloning within an assertion disables the clippy
        // allow, so no assertion here.
        #[allow(clippy::clone_on_copy)]
        let _ = Units(1).clone();

        // Test Default
        assert_eq!(Units::default(), Units(0));

        // Test Ord, PartialOrd
        assert!(Units::default() < Units(1));
        assert!(!(Units(1) < Units::default()));

        // Test Copy
        let z = Units(3);
        let mut d = z;
        d += Units(1);
        assert_eq!(z, Units(3));
        assert_eq!(d, Units(4));
    }

    #[test]
    /// Test non-arithmetic derivations that are derived explicitly
    fn test_explicit_derivations() {
        // Test From
        assert_eq!(Units::from(3) + Units(3), Units(6));

        // Test Deref
        assert_eq!(*Units(3) + 3, 6);

        // Test Debug
        assert_eq!(format!("{:?}", Units(3)), "Units(3)");

        // Test Display
        assert_eq!(format!("{:}", Units(3)), "3 units");
    }

    #[test]
    /// Test operations that work on an iterator of values, like sum and
    /// product.
    fn test_summary_operations() {
        // Test Sum
        assert_eq!(
            vec![Units(2), Units(3)].iter().cloned().sum::<Units>(),
            Units(5)
        );
    }

    #[test]
    /// Test addition.
    fn test_addition() {
        assert_eq!(Units(1) + Units(3), Units(4));

        let mut z = Units(1);
        z += Units(3);
        assert_eq!(z, Units(4));

        assert_eq!(Units(u64::MAX).checked_add(Units(1)), None);
    }

    #[test]
    /// Test subtraction
    fn test_subtraction() {
        assert_eq!(Units(3) - Units(1), Units(2));

        let mut z = Units(3);
        z -= Units(1);
        assert_eq!(z, Units(2));
    }

    #[test]
    /// Test multiplication
    fn test_multiplication() {
        assert_eq!(Units(3) * 2u8, Units(6));
        assert_eq!(2u8 * Units(3), Units(6));

        assert_eq!(Units(3) * 2u16, Units(6));
        assert_eq!(2u16 * Units(3), Units(6));

        assert_eq!(Units(3) * 2u32, Units(6));
        assert_eq!(2u32 * Units(3), Units(6));

        assert_eq!(Units(3) * 2u64, Units(6));
        assert_eq!(2u64 * Units(3), Units(6));

        assert_eq!(Units(3) * 2usize, Units(6));
        assert_eq!(2usize * Units(3), Units(6));
    }

    #[test]
    /// Test division and remainder together
    fn test_division_and_remainder() {
        assert_eq!(Units(3) / Units(2), 1);
        assert_eq!(Units(3) % Units(2), Units(1));

        assert_eq!(Units(5) / 2u8, Units(2));
        assert_eq!(Units(3) % 2u8, Units(1));

        assert_eq!(Units(5) / 2u16, Units(2));
        assert_eq!(Units(3) % 2u16, Units(1));

        assert_eq!(Units(5) / 2u32, Units(2));
        assert_eq!(Units(3) % 2u32, Units(1));

        assert_eq!(Units(5) / 2u64, Units(2));
        assert_eq!(Units(3) % 2u64, Units(1));

        assert_eq!(Units(5) / 2usize, Units(2));
        assert_eq!(Units(3) % 2usize, Units(1));
    }
}