//! Handles all parsing of expressions.
//! An expression is essentially any piece of code returning a value.

use nom::Parser;

use super::{atom, util, IResult};

use crate::ast::*;

/// The main entry point of the module.
/// Attempts to parse an expression from the input.
///
/// Where an expression take the following form:
/// expr => binop | term
pub fn parse_expression(input: &[u8]) -> IResult<Expression> {
    parse_binary_operation_or_term(input)
}

fn parse_nested_expression(input: &[u8]) -> IResult<Expression> {
    util::ws(nom::sequence::delimited(
        nom::character::complete::char('('),
        parse_expression,
        nom::character::complete::char(')'),
    ))(input)
}

#[test]
fn test_parse_nested_expression() {
    let (input, expr) = parse_nested_expression(b"( 100 + 201 )").unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::IntegerLiteral(100)),
            Box::new(Expression::IntegerLiteral(201))
        )
    );
    assert_eq!(input, b"");
}

/// Attempts to parse either a binary operation or single term from the input.
///
/// A binary operation looks like so:
/// binop => term op term
/// term => literal | ( expr )
fn parse_binary_operation_or_term(input: &[u8]) -> IResult<Expression> {
    fn _build_binary_operation_parser<'p>(
        precedence_level: usize,
        precedence_levels: &'static [&'static [Operator]],
    ) -> impl Fn(&'p [u8]) -> IResult<'p, Expression> + 'p {
        let num_precedence_levels = precedence_levels.len();

        move |input: &'p [u8]| -> IResult<'p, Expression> {
            let operators = if precedence_level < num_precedence_levels {
                Some(precedence_levels[precedence_level].clone())
            } else {
                None
            };

            let next_level = |input: &'p [u8]| {
                if precedence_level < num_precedence_levels {
                    _build_binary_operation_parser(precedence_level + 1, precedence_levels)(input)
                } else {
                    nom::branch::alt((atom::parse_integer_literal, parse_nested_expression))(input)
                }
            };

            if operators.is_none() {
                return next_level(input);
            }

            let parse_op_chain = nom::multi::many0(
                atom::build_operator_parser(operators).and(super::error::expect(next_level)),
            );

            let res = nom::error::context(
                "parsing expression",
                nom::combinator::map(
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
                ),
            )(input);

            res
        }
    }

    let precedence_levels = Operator::precedence_levels();
    _build_binary_operation_parser(0, precedence_levels)(input)
}

#[test]
fn test_parse_binary_operation() {
    // Test parsing simple addition
    let (input, expr) = parse_binary_operation_or_term(b"100+201").unwrap();
    assert_eq!(
        expr,
        Expression::BinaryOperation(
            Operator::Add,
            Box::new(Expression::IntegerLiteral(100)),
            Box::new(Expression::IntegerLiteral(201))
        )
    );
    assert_eq!(input, b"");
    // Test with three terms
    let (input, expr) = parse_binary_operation_or_term(b"100+200+300").unwrap();
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
    assert_eq!(input, b"");
    // Test order of operations, with operators of the same precedence.
    let (input, expr) = parse_binary_operation_or_term(b"100-101+200").unwrap();
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
    assert_eq!(input, b"");
    // Test order of operations with operators of different precedence
    let (input, expr) = parse_binary_operation_or_term(b"100/2+300/4").unwrap();
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
    assert_eq!(input, b"");

    let (input, expr) = parse_binary_operation_or_term(b"10 * (100 + 201)").unwrap();
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
    assert_eq!(input, b"");
}
