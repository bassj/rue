use std::collections::HashMap;

use lazy_static::lazy_static;
use phf::phf_map;

pub type Node = Expression;

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i64),
    BinaryOperation(Operator, Box<Expression>, Box<Expression>),
    NestedExpression(Box<Expression>),
    NoOp
}

impl Default for Expression {
    fn default() -> Self {
        Self::NoOp
    }
}

static OPERATOR_MAP: phf::Map<char, Operator> = phf_map! {
    '+' => Operator::Add,
    '-' => Operator::Subtract,
    '*' => Operator::Multiply,
    '/' => Operator::Divide,
};

lazy_static! {
    static ref OPERATOR_TOKENS: String = {
        OPERATOR_MAP.keys().collect()
    };

    static ref INVERSE_OPERATOR_MAP: HashMap<Operator, char> = {
        let mut map = HashMap::new();

        for entry in OPERATOR_MAP.entries() {
            map.insert(*entry.1, *entry.0);
        }

        map
    }; 
}

#[derive(Hash, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
}

lazy_static! {
    static ref PRECEDENCE_LEVELS: Vec<Vec<Operator>> = vec![
        vec![Operator::Add, Operator::Subtract],
        vec![Operator::Multiply, Operator::Divide],
    ];
}

impl<'a> Operator {
    pub fn tokens() -> String {
        OPERATOR_TOKENS.clone()
    }

    pub fn precedence_levels() -> &'static Vec<Vec<Operator>> {
        &PRECEDENCE_LEVELS
    }

    pub fn get_token(&self) -> char {
        *INVERSE_OPERATOR_MAP.get(self).unwrap()
    }
}

impl std::convert::TryFrom<char> for Operator {
    type Error = ();

    fn try_from(c: char) -> Result<Self, Self::Error> {
        if OPERATOR_MAP.contains_key(&c) {
            Ok(
                *OPERATOR_MAP.get(&c).unwrap()
            )
        } else {
            Err(())
        }
    }
}