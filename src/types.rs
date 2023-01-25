use impls::impls;
use num_traits::{int::PrimInt, NumCast, Signed};
use std::ops::{Add, Div, Mul, Sub};

const BITS_PER_BYTE: u32 = 8;

/// Rue's representation of an integer.
#[derive(Debug)]
pub struct RueInteger {
    /// The number of bits that the number type should contain
    pub bit_width: u32,
    /// Whether the first bit of the number should represent it's sign
    pub signed: bool,
    /// signed 128 bit representation of this integer's value.
    pub value: i128,
}
impl RueInteger {
    /// Shrinks the bit_width to the smallest value which can still contain the stored integer.
    /// Doesn't shrink smaller than a bit width of 32
    pub fn shrink(mut self) -> Self {
        let new_bit_width = match self.signed {
            true => {
                if self.value <= i32::MAX.into() {
                    32
                } else if self.value <= i64::MAX.into() {
                    64
                } else {
                    128
                }
            }
            false => unimplemented!(),
        };

        self.bit_width = new_bit_width;
        self
    }
}

impl Add for RueInteger {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            bit_width: std::cmp::max(self.bit_width, rhs.bit_width),
            signed: self.signed || rhs.signed,
            value: self.value + rhs.value,
        }
    }
}

impl Sub for RueInteger {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            bit_width: std::cmp::max(self.bit_width, rhs.bit_width),
            signed: self.signed || rhs.signed,
            value: self.value - rhs.value,
        }
    }
}

impl Mul for RueInteger {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            bit_width: std::cmp::max(self.bit_width, rhs.bit_width),
            signed: self.signed || rhs.signed,
            value: self.value * rhs.value,
        }
    }
}

impl Div for RueInteger {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            bit_width: std::cmp::max(self.bit_width, rhs.bit_width),
            signed: self.signed || rhs.signed,
            value: self.value / rhs.value,
        }
    }
}

impl<T: PrimInt> From<T> for RueInteger {
    fn from(value: T) -> Self {
        RueInteger {
            bit_width: std::mem::size_of::<T>() as u32 * BITS_PER_BYTE,
            signed: impls!(T: Signed),
            value: NumCast::from(value).expect("unable to convert value to i128"),
        }
    }
}

impl PartialEq for RueInteger {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

type RueTypeError = ();

#[derive(Debug, PartialEq)]
pub enum RueValue {
    Integer(RueInteger),
}
impl RueValue {
    pub fn try_add(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs + rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_sub(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs - rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_mul(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs * rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_div(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs / rhs).into()),
            _ => Err(()),
        }
    }
}

impl From<RueInteger> for RueValue {
    fn from(value: RueInteger) -> Self {
        RueValue::Integer(value)
    }
}

impl TryFrom<RueValue> for u64 {
    type Error = ();

    fn try_from(value: RueValue) -> Result<Self, Self::Error> {
        match value {
            RueValue::Integer(rue_int) => rue_int.value.try_into().map_err(|err| ()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests the implementation of From on RueInteger over all of the rust standard integer primitive types.
    #[test]
    fn test_rue_integer_from_int_prims() {
        let value = RueInteger::from(100i8);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 8,
                signed: true,
                value: 100
            }
        );

        let value = RueInteger::from(100i16);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 16,
                signed: true,
                value: 100
            }
        );

        let value = RueInteger::from(100i32);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 32,
                signed: true,
                value: 100
            }
        );

        let value = RueInteger::from(100i64);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 64,
                signed: true,
                value: 100
            }
        );

        let value = RueInteger::from(100i128);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 128,
                signed: true,
                value: 100
            }
        );
        
        let value = RueInteger::from(100u8);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 8,
                signed: false,
                value: 100
            }
        );

        let value = RueInteger::from(100u16);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 16,
                signed: false,
                value: 100
            }
        );

        let value = RueInteger::from(100u32);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 32,
                signed: false,
                value: 100
            }
        );

        let value = RueInteger::from(100u64);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 64,
                signed: false,
                value: 100
            }
        );

        let value = RueInteger::from(100u128);
        assert_eq!(
            value,
            RueInteger {
                bit_width: 128,
                signed: false,
                value: 100
            }
        );
    }
}
