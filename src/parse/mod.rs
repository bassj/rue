mod atom;
mod error;
mod expr;
mod func;
mod statement;
mod util;

use nom::Err;

use nom_locate::LocatedSpan;

use crate::ast;

use error::ParseError;

type InputType<'p> = LocatedSpan<&'p str>;
type IResult<'p, T> = nom::IResult<InputType<'p>, T, ParseError<'p>>;

/// The main entry point for parsing a rue program
pub fn parse_source<'s, T: Into<&'s str>>(input: T) -> Result<Vec<ast::Statement>, ParseError<'s>> {
    let src_string = input.into();
    let src = LocatedSpan::new(src_string.as_ref());

    let res = nom::combinator::all_consuming(nom::multi::many0(statement::parse_statement))(src);

    match res {
        Ok((_, stmt)) => Ok(stmt),
        Err(e) => match e {
            Err::Failure(e) => Err(e),
            Err::Error(e) => Err(e),
            _ => unimplemented!(),
        },
    }
}
