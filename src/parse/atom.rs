//! Handles parsing of all "atoms".
//! An atom is a leaf node of our abstract syntax tree.

use nom::Slice;

use crate::ast::{Expression, Operator};

use super::{util, IResult, InputType};

/// Constructrs a closure that can be used to parse operators out of our input stream.
pub fn build_operator_parser(
    operators: Option<&[Operator]>,
) -> impl Fn(InputType) -> IResult<Operator> {
    let operators: Option<String> =
        operators.map(|o| o.into_iter().map(|op| -> char { op.into() }).collect());

    fn parse_any_operator(input: InputType) -> IResult<Operator> {
        // TODO: See if we can force this to panic. Not sure if we should actually handle the no character case.
        Operator::try_from(
            input
                .chars()
                .next()
                .expect("Unexpected EOF parsing operator"),
        )
        .map(|op| (input.slice(1..), op))
        .map_err(|_| nom::Err::Error(super::error::ParseError::default()))
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
    use nom_locate::LocatedSpan;

    let addition_parser = build_operator_parser(Some(&[Operator::Add]));

    let (input, operator) = addition_parser(LocatedSpan::new("+")).unwrap();
    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned correct input string"
    );
    assert_eq!(
        operator,
        Operator::Add,
        "Parser returned correct operator type"
    );

    let result = addition_parser(LocatedSpan::new("-"));
    assert!(
        result.is_err(),
        "Addition parser returns an error trying to parse '-'"
    );

    let addition_subtraction_parser =
        build_operator_parser(Some(&[Operator::Add, Operator::Subtract]));

    let (input, operator) = addition_parser(LocatedSpan::new("+")).unwrap();
    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned correct input string"
    );
    assert_eq!(
        operator,
        Operator::Add,
        "Parser returned correct operator type"
    );

    let (input, operator) = addition_subtraction_parser(LocatedSpan::new("-")).unwrap();
    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned correct input string"
    );
    assert_eq!(
        operator,
        Operator::Subtract,
        "Parser returned correct operator type"
    );
}

/// Attempts to parse an integer literal out of our input stream.
pub fn parse_integer_literal(input: InputType) -> IResult<Expression> {
    nom::combinator::map_res(
        util::ws(nom::character::complete::digit1),
        |value: InputType| -> Result<Expression, Box<dyn std::error::Error>> {
            let value = value.parse::<i64>()?;
            Ok(Expression::IntegerLiteral(value))
        },
    )(input)
}

#[test]
fn test_parse_integer_literal() {
    use nom_locate::LocatedSpan;

    let (input, expr) = parse_integer_literal(LocatedSpan::new("100")).unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(
        input.fragment(),
        &"",
        "Integer literal was removed from input"
    );

    let (input, expr) = parse_integer_literal(LocatedSpan::new("0")).unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(0),
        "Integer literal was correctly parsed"
    );
    assert_eq!(
        input.fragment(),
        &"",
        "Integer literal was removed from input"
    );

    let (input, expr) = parse_integer_literal(LocatedSpan::new("100 200")).unwrap();
    assert_eq!(
        expr,
        Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(
        input.fragment(),
        &"200",
        "Integer literal was removed from input"
    );

    let res = parse_integer_literal(LocatedSpan::new("not an int"));
    assert!(res.is_err(), "Trying to parse non-operator causes error");

    let res = parse_integer_literal(LocatedSpan::new(""));
    assert!(res.is_err(), "Failing to parse empty string causes error");
}
