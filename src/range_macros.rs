// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// An omnibus macro that includes all simple macros.
macro_rules! range {
    ($T: ident) => {
        macro_attr! {
            #[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
            /// A type for $T
            pub struct $T(pub u64);
        }

        checked_add!($T);

        NewtypeAdd! { () pub struct $T(u64); }
        NewtypeAddAssign! { () pub struct $T(u64); }
        NewtypeDeref! { () pub struct $T(u64); }
        NewtypeFrom! { () pub struct $T(u64); }
        NewtypeSub! { () pub struct $T(u64); }
        NewtypeSubAssign! { () pub struct $T(u64); }

        debug!($T);
        rem!($T);
        self_div!($T);
        serde!($T);
        sum!($T);

        unsigned_div!(u64, $T);
        unsigned_div!(u32, $T);
        unsigned_div!(u16, $T);
        unsigned_div!(u8, $T);
        usize_div!($T);

        unsigned_mul!(u64, $T);
        unsigned_mul!(u32, $T);
        unsigned_mul!(u16, $T);
        unsigned_mul!(u8, $T);
        usize_mul!($T);

        unsigned_rem!(u64, $T);
        unsigned_rem!(u32, $T);
        unsigned_rem!(u16, $T);
        unsigned_rem!(u8, $T);
        usize_rem!($T);
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

macro_rules! debug {
    ($T:ident) => {
        impl fmt::Debug for $T {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, stringify!($T))?;
                write!(f, "({})", **self)
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

macro_rules! rem {
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
