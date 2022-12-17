mod atom;
mod error;
mod expr;
mod util;

use nom::error::VerboseError;

use crate::ast;

type InputType<'p> = &'p [u8];
type IResult<'p, T> = nom::IResult<InputType<'p>, T, VerboseError<InputType<'p>>>;

/// The main entry point for parsing a rue program
pub fn parse_source<T: Into<String>>(input: T) -> Result<ast::Expression, error::ParseError> {
    let src_string = input.into();

    let (_, expr) =
        expr::parse_expression(src_string.as_bytes()).map_err(error::build_parse_error)?;

    Ok(expr)
}
