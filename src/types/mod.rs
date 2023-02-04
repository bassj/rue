const BITS_PER_BYTE: u32 = 8;

mod integer;

pub use integer::RueInteger;

#[derive(Debug, PartialEq)]
pub enum RueType {
    Integer { bit_width: u32, signed: bool },
    Implicit,
    Unit,
}

impl From<&str> for RueType {
    fn from(value: &str) -> Self {
        match value {
            "i8" => RueType::Integer {
                bit_width: 8,
                signed: true,
            },
            "i16" => RueType::Integer {
                bit_width: 16,
                signed: true,
            },
            "i32" => RueType::Integer {
                bit_width: 32,
                signed: true,
            },
            "i64" => RueType::Integer {
                bit_width: 64,
                signed: true,
            },
            "i128" => RueType::Integer {
                bit_width: 128,
                signed: true,
            },
            "u8" => RueType::Integer {
                bit_width: 8,
                signed: false,
            },
            "u16" => RueType::Integer {
                bit_width: 16,
                signed: false,
            },
            "u32" => RueType::Integer {
                bit_width: 32,
                signed: false,
            },
            "u64" => RueType::Integer {
                bit_width: 64,
                signed: false,
            },
            "u128" => RueType::Integer {
                bit_width: 128,
                signed: false,
            },
            str => panic!("Unknown type: {}", str),
        }
    }
}

impl From<&RueValue> for RueType {
    fn from(value: &RueValue) -> Self {
        match value {
            RueValue::Integer(int_value) => RueType::from(int_value),
        }
    }
}

type RueTypeError = ();

// I have a thought that this type could simply be a RueType and a vector of bytes / u128.
// Need to think through how that might affect things. I kinda like using the rust type system
// To represent rue types. It makes some guarantees at the cost of flexibility.
#[derive(Debug, PartialEq)]
pub enum RueValue {
    Integer(RueInteger),
}
impl RueValue {
    pub fn try_add(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        #[allow(unreachable_patterns)]
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs + rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_sub(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        #[allow(unreachable_patterns)]
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs - rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_mul(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        #[allow(unreachable_patterns)]
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs * rhs).into()),
            _ => Err(()),
        }
    }

    pub fn try_div(lhs: Self, rhs: Self) -> Result<Self, RueTypeError> {
        #[allow(unreachable_patterns)]
        match (lhs, rhs) {
            (RueValue::Integer(lhs), RueValue::Integer(rhs)) => Ok((lhs / rhs).into()),
            _ => Err(()),
        }
    }
}

impl TryFrom<RueValue> for u64 {
    type Error = ();

    fn try_from(value: RueValue) -> Result<Self, Self::Error> {
        match value {
            RueValue::Integer(rue_int) => rue_int.value.try_into().map_err(|_err| ()),
        }
    }
}
