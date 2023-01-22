#[derive(Debug, PartialEq)]
pub enum Statement {
    VariableDeclaration(String, Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    FunctionInvocation(String, Vec<Expression>),
    IntegerLiteral(i64),
    Variable(String),
    BinaryOperation(Operator, Box<Expression>, Box<Expression>),
    NoOp
}
impl Expression {
    /// returns true if this expression represents a constant value
    pub fn is_constant(&self) -> bool {
        match self {
            Self::FunctionInvocation(_, _) => false,
            Self::Variable(_) => false,
            Self::BinaryOperation(_, lhs, rhs) => { lhs.is_constant() && rhs.is_constant() },
            _ => true,
        } 
    }

    /// compute the value of this expression.
    /// Panics if the expression is not constant
    pub fn compute_value(self) -> i64 {
        match self {
            Self::BinaryOperation(op, lhs, rhs) => { 
                evaulate_expression(
                    Expression::BinaryOperation(op, lhs, rhs)
                )
            },
            Self::IntegerLiteral(val) => {
                val
            }
            _ => panic!("Attempting to compute constant from non-constant value"),
        } 
    }
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

fn evaulate_expression(expr: Expression) -> i64 {
    match expr {
        Expression::IntegerLiteral(i) => i,
        Expression::NoOp => 0,
        Expression::BinaryOperation(op, lhs, rhs) => {
            let lhs = evaulate_expression(*lhs);
            let rhs = evaulate_expression(*rhs);
            match op {
                Operator::Add => { lhs + rhs },
                Operator::Subtract => { lhs - rhs },
                Operator::Multiply => { lhs * rhs },
                Operator::Divide => { lhs / rhs },
            }
        },
        _ => unimplemented!(),
    }
}