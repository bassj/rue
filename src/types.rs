use std::ops::{Add, Sub, Div, Mul};

const BITS_PER_BYTE: u32 = 8;

trait Integer {
    fn is_signed() -> bool;
    fn coerce(self) -> i128;
}

impl Integer for i8 {
    fn is_signed() -> bool {
        true
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for i16 {
    fn is_signed() -> bool {
        true
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for i32 {
    fn is_signed() -> bool {
        true
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for i64 {
    fn is_signed() -> bool {
        true
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for u8 {
    fn is_signed() -> bool {
        false
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for u16 {
    fn is_signed() -> bool {
        false
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for u32 {
    fn is_signed() -> bool {
        false
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for u64 {
    fn is_signed() -> bool {
        false
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}

impl Integer for u128 {
    fn is_signed() -> bool {
        false
    }

    fn coerce(self) -> i128 {
        self as i128
    }
}


#[derive(Debug)]
pub struct RueInteger {
    pub bit_width: u32,
    pub signed: bool,
    pub value: i128,
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

impl<T: Integer> From<T> for RueInteger {
    fn from(value: T) -> Self {
        RueInteger {
            bit_width: std::mem::size_of::<T>() as u32 * BITS_PER_BYTE,
            signed: T::is_signed(),
            value: value.coerce(),
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
            _ => Err(())
        }
    }

    pub fn try_sub(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs - rhs).into()),
            _ => Err(())
        }
    }

    pub fn try_mul(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs * rhs).into()),
            _ => Err(())
        }
    }

    pub fn try_div(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs / rhs).into()),
            _ => Err(())
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