//! Handles parsing of all "atoms".
//! An atom is a leaf node of our abstract syntax tree.

use nom::AsChar;

use crate::ast::{Expression, Operator};

use super::{util, IResult, InputType};

/// Constructrs a closure that can be used to parse operators out of our input stream.
pub fn build_operator_parser(
    operators: Option<&[Operator]>,
) -> impl Fn(InputType) -> IResult<Operator> {
    let operators: Option<String> =
        operators.map(|o| o.into_iter().map(|op| -> char { op.into() }).collect());

    fn parse_any_operator(input: InputType) -> IResult<Operator> {
        Operator::try_from(input[0].as_char())
            .map(|op| (&input[1..], op))
            .map_err(|_err| {
                nom::Err::Error(nom::error::VerboseError {
                    errors: vec![(
                        input,
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::OneOf),
                    )],
                })
            })
    }

    move |input: InputType| {
        match &operators {
            Some(ops) => {
                util::ws(nom::combinator::map(
                    nom::character::complete::one_of(ops.as_str()),
                    |c: char| Operator::try_from(c).unwrap(),
                    // This shouldn't fail, since the parser can only match valid characters
                ))(input)
            }
            _ => util::ws(parse_any_operator)(input),
        }
    }
}

#[test]
fn test_parse_operator() {
    let addition_parser = build_operator_parser(Some(&[Operator::Add]));

    let (input, operator) = addition_parser(b"+").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Add,
        "Parser returned correct operator type"
    );

    let result = addition_parser(b"-");
    assert!(
        result.is_err(),
        "Addition parser returns an error trying to parse '-'"
    );

    let addition_subtraction_parser =
        build_operator_parser(Some(&[Operator::Add, Operator::Subtract]));

    let (input, operator) = addition_parser(b"+").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Add,
        "Parser returned correct operator type"
    );

    let (input, operator) = addition_subtraction_parser(b"-").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Subtract,
        "Parser returned correct operator type"
    );
}

/// Attempts to parse an integer literal out of our input stream.
pub fn parse_integer_literal(input: &[u8]) -> IResult<Expression> {
    nom::combinator::map_res(
        util::ws(nom::bytes::complete::take_while(nom::character::is_digit)),
        |value: &[u8]| -> Result<Expression, Box<dyn std::error::Error>> {
            let value = std::str::from_utf8(value)?.to_string();
            let value = value.parse::<i64>()?;
            Ok(Expression::IntegerLiteral(value))
        },
    )(input)
}

#[test]
fn test_parse_integer_literal() {
    let (input, expr) = parse_integer_literal(b"100").unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b"", "Integer literal was removed from input");

    let (input, expr) = parse_integer_literal(b"0").unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(0),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b"", "Integer literal was removed from input");

    let (input, expr) = parse_integer_literal(b"100 200").unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b"200", "Integer literal was removed from input");

    let res = parse_integer_literal(b"not an int");
    assert!(res.is_err(), "Trying to parse non-operator causes error");

    let res = parse_integer_literal(b"");
    assert!(res.is_err(), "Failing to parse empty string causes error");
}
