
#[derive(Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i64),
    BinaryOperation(Operator, Box<Expression>, Box<Expression>),
    NoOp
}

impl Default for Expression {
    fn default() -> Self {
        Self::NoOp
    }
}

#[derive(Hash, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
}

static PRECEDENCE_LEVELS: &'static [&'static [Operator]] = &[
    &[Operator::Add, Operator::Subtract],
    &[Operator::Multiply, Operator::Divide]
];

impl Operator {
    pub fn precedence_levels() -> &'static [&'static [Operator]] {
        PRECEDENCE_LEVELS
    }
}

impl std::convert::TryFrom<char> for Operator {
    type Error = ();

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '+' => Ok(Operator::Add),
            '-' => Ok(Operator::Subtract),
            '*' => Ok(Operator::Multiply),
            '/' => Ok(Operator::Divide),
            _ => Err(()),
        }
    }
}

impl std::convert::From<&Operator> for char {
    fn from(op: &Operator) -> Self {
        match op {
            &Operator::Add => '+',
            &Operator::Subtract => '-',
            &Operator::Multiply => '*',
            &Operator::Divide => '/',
        }
    }
}