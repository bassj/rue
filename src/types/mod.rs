const BITS_PER_BYTE: u32 = 8;

mod integer;

pub use integer::RueInteger;

#[derive(Debug, PartialEq)]
pub enum RueType {
    Integer {
        bit_width: u32,
        signed: bool,
    },
    /// The implicit type, isn't really a type, it just means "Try and figure out what type i mean at compile time"
    Implicit,
    /// The unit type is meant to be like rust's unit type, similar to a void type in other languages.
    Unit,
}

impl RueType {

    pub const I8: RueType = RueType::Integer {
        bit_width: 8,
        signed: true,
    };

    pub const I16: RueType = RueType::Integer {
        bit_width: 16,
        signed: true,
    };

    pub const I32: RueType = RueType::Integer {
        bit_width: 32,
        signed: true,
    };

    pub const I64: RueType = RueType::Integer {
        bit_width: 64,
        signed: true,
    };

    pub const I128: RueType = RueType::Integer {
        bit_width: 128,
        signed: true,
    };


    pub const U8: RueType = RueType::Integer {
        bit_width: 8,
        signed: false,
    };

    pub const U16: RueType = RueType::Integer {
        bit_width: 16,
        signed: false,
    };

    pub const U32: RueType = RueType::Integer {
        bit_width: 32,
        signed: false,
    };

    pub const U64: RueType = RueType::Integer {
        bit_width: 64,
        signed: false,
    };

    pub const U128: RueType = RueType::Integer {
        bit_width: 128,
        signed: false,
    };
}

impl From<&str> for RueType {
    fn from(value: &str) -> Self {
        match value {
            "i8" => RueType::I8,
            "i16" => RueType::I16,
            "i32" => RueType::I32,
            "i64" => RueType::I64,
            "i128" => RueType::I128,
            "u8" => RueType::U8,
            "u16" => RueType::U16,
            "u32" => RueType::U32,
            "u64" => RueType::U64,
            "u128" => RueType::U128,
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
