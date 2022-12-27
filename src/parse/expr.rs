//! Handles all parsing of expressions.
//! An expression is essentially any piece of code returning a value.

use std::{cell::RefCell, rc::Rc};

use nom::Parser;

use super::{atom, error, error::ParseError, util, IResult, InputType};

use crate::ast::*;

/// The main entry point of the module.
/// Attempts to parse an expression from the input.
///
/// Where an expression take the following form:
/// expr => binop | term
pub fn parse_expression(input: InputType) -> IResult<(Expression, Vec<ParseError>)> {
    parse_binary_operation_or_term(input)
}

fn parse_nested_expression(input: InputType) -> IResult<(Expression, Vec<ParseError>)> {
    util::ws(util::delimited_preserve_errors(
        nom::character::complete::char('('),
        parse_expression,
        nom::character::complete::char(')'),
    ))(input)
}

#[test]
fn test_parse_nested_expression() {
    use nom_locate::LocatedSpan;

    let (input, (expr, _)) = parse_nested_expression(LocatedSpan::new("( 100 + 201 )")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::IntegerLiteral(100)),
            Box::new(Expression::IntegerLiteral(201))
        )
    );
    assert_eq!(input.fragment(), &"");
}

#[test]
fn test_parse_nested_expression_error() {
    use nom_locate::LocatedSpan;

    let res = parse_nested_expression(LocatedSpan::new("( 100 "));
    assert!(
        res.is_err(),
        "Parsing invalid nested expression returned an error"
    );
    let err = match res.unwrap_err() {
        nom::Err::Error(e) => e,
        nom::Err::Failure(e) => e,
        _ => panic!("Shouldn't be incomplete"),
    };

    let (short_message, long_message) = err.error_message();

    assert_eq!(short_message, "expected `)`", "Error has correct message");

    assert_eq!(
        long_message, "expected either `)` or an operator, found ``",
        "Error has correct message"
    );

    println!("{}", err);
}

/// Attempts to parse either a binary operation or single term from the input.
///
/// A binary operation looks like so:
/// binop => term op term
/// term => literal | ( expr )
fn parse_binary_operation_or_term<'p>(input: InputType) -> IResult<(Expression, Vec<ParseError>)> {
    fn _build_binary_operation_parser<'p>(
        precedence_level: usize,
        precedence_levels: &'static [&'static [Operator]],
        error_stack: Rc<RefCell<Vec<ParseError<'p>>>>,
    ) -> impl FnMut(InputType<'p>) -> IResult<'p, Expression> + 'p {
        let num_precedence_levels = precedence_levels.len();

        move |input: InputType<'p>| -> IResult<'p, Expression> {
            let operators = if precedence_level < num_precedence_levels {
                Some(precedence_levels[precedence_level].clone())
            } else {
                None
            };

            let next_level = |input: InputType<'p>| {
                if precedence_level < num_precedence_levels {
                    _build_binary_operation_parser(
                        precedence_level + 1,
                        precedence_levels,
                        error_stack.clone(),
                    )(input)
                } else {
                    nom::branch::alt((
                        atom::parse_integer_literal,
                        parse_nested_expression.map(|(expr, nested_err_stack)| {
                            error_stack.borrow_mut().extend(nested_err_stack);
                            expr
                        }),
                    ))(input)
                }
            };

            if operators.is_none() {
                return next_level(input);
            }

            let parse_op_chain = nom::multi::many0(error::preserve(
                error_stack.clone(),
                atom::build_operator_parser(operators).and(super::error::expect(next_level)),
            ));

            let res = nom::combinator::map(
                nom::sequence::tuple((next_level, parse_op_chain)),
                move |(mut lhs, op_chain)| {
                    if op_chain.len() == 0 {
                        lhs
                    } else {
                        for (op, rhs) in op_chain {
                            let _lhs = std::mem::take(&mut lhs);
                            let mut expr =
                                Expression::BinaryOperation(op, Box::new(_lhs), Box::new(rhs));
                            std::mem::swap(&mut lhs, &mut expr);
                        }

                        lhs
                    }
                },
            )(input);

            res
        }
    }

    let error_stack = Rc::new(RefCell::new(Vec::new()));
    let precedence_levels = Operator::precedence_levels();
    let res = _build_binary_operation_parser(0, precedence_levels, error_stack.clone())(input);

    let error_stack =
        Rc::try_unwrap(error_stack).expect("Failed to release all references of error_stack");
    let error_stack = error_stack.into_inner();

    res.map(|(i, expr)| (i, (expr, error_stack)))
}

#[test]
fn test_parse_binary_operation() {
    use nom_locate::LocatedSpan;

    // Test parsing simple addition
    let (input, (expr, _)) = parse_binary_operation_or_term(LocatedSpan::new("100+201")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::IntegerLiteral(100)),
            Box::new(Expression::IntegerLiteral(201))
        )
    );
    assert_eq!(input.fragment(), &"");
    // Test with three terms
    let (input, (expr, _)) =
        parse_binary_operation_or_term(LocatedSpan::new("100+200+300")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::BinaryOperation(
                Operator::Add,
                Box::new(Expression::IntegerLiteral(100)),
                Box::new(Expression::IntegerLiteral(200))
            )),
            Box::new(Expression::IntegerLiteral(300)),
        )
    );
    assert_eq!(input.fragment(), &"");
    // Test order of operations, with operators of the same precedence.
    let (input, (expr, _)) =
        parse_binary_operation_or_term(LocatedSpan::new("100-101+200")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::BinaryOperation(
                Operator::Subtract,
                Box::new(Expression::IntegerLiteral(100)),
                Box::new(Expression::IntegerLiteral(101)),
            )),
            Box::new(Expression::IntegerLiteral(200)),
        )
    );
    assert_eq!(input.fragment(), &"");
    // Test order of operations with operators of different precedence
    let (input, (expr, _)) =
        parse_binary_operation_or_term(LocatedSpan::new("100/2+300/4")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::BinaryOperation(
                Operator::Divide,
                Box::new(Expression::IntegerLiteral(100)),
                Box::new(Expression::IntegerLiteral(2))
            )),
            Box::new(Expression::BinaryOperation(
                Operator::Divide,
                Box::new(Expression::IntegerLiteral(300)),
                Box::new(Expression::IntegerLiteral(4))
            ))
        )
    );
    assert_eq!(input.fragment(), &"");

    let (input, (expr, _)) =
        parse_binary_operation_or_term(LocatedSpan::new("10 * (100 + 201)")).unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Multiply,
            Box::new(Expression::IntegerLiteral(10)),
            Box::new(Expression::BinaryOperation(
                Operator::Add,
                Box::new(Expression::IntegerLiteral(100)),
                Box::new(Expression::IntegerLiteral(201))
            )),
        ),
    );
    assert_eq!(input.fragment(), &"");
}

// TODO: The premise for this test is incorrect
// #[test]
// fn test_parse_binary_operation_err() {
//     use nom_locate::LocatedSpan;

//     let res = parse_binary_operation_or_term(LocatedSpan::new("10 * a"));
//     assert!(res.is_err(), "Parsing invalid binary operation returns an error");

//     let err = match res.unwrap_err() {
//         nom::Err::Error(e) => e,
//         nom::Err::Failure(e) => e,
//         _ => panic!("Shouldn't be incomplete"),
//     };

//     assert_eq!(err.error_message(), "expected expression", "Error has correct short message");
//     assert_eq!(err.error_message_long(), "expected expression, found `a`", "Error has correct short message");

// }
