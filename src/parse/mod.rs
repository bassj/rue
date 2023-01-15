mod atom;
mod error;
mod expr;
mod util;
mod func;

use nom::Err;

use nom_locate::LocatedSpan;

use crate::ast::{self, Expression};

use error::ParseError;

type InputType<'p> = LocatedSpan<&'p str>;
type IResult<'p, T> = nom::IResult<InputType<'p>, T, ParseError<'p>>;

/// The main entry point for parsing a rue program
pub fn parse_source<'s, T: Into<&'s str>>(input: T) -> Result<ast::Expression, ParseError<'s>> {
    let src_string = input.into();
    let src = LocatedSpan::new(src_string.as_ref());

    let res = nom::combinator::all_consuming(parse_statement)(src);

    match res {
        Ok((_, expr)) => Ok(expr),
        Err(e) => match e {
            Err::Failure(e) => Err(e),
            Err::Error(e) => Err(e),
            _ => Err(ParseError::default()),
        },
    }
}

fn parse_statement(input: InputType) -> IResult<Expression> {
    let t = nom_preserve::sequence::terminated(
        nom_preserve::error::blame(expr::parse_expression),
        nom::branch::alt((nom::character::complete::line_ending, nom::combinator::eof)),
    )(input);

    t.map(|(i, (expr, _err_stack))| (i, expr))
}
