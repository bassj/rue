mod atom;
mod expr;
mod util;

use crate::ast;

type InputType<'p> = &'p [u8];
type IResult<'p, T> = nom::IResult<InputType<'p>, T>;

/// The main entry point for parsing a rue program
pub fn parse_source<T: Into<String>>(input: T) -> ast::Expression {
    let src_string = input.into();

    let (_, expr) = expr::parse_expression(src_string.as_bytes()).expect("Error parsing expression");
    expr
}