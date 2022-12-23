mod atom;
mod error;
mod expr;
mod util;

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

    let res = nom::error::context(
        "parsing source",
        nom::combinator::all_consuming(parse_statement),
    )(src);

    match res {
        Ok((_, expr)) => Ok(expr),
        Err(e) => match e {
            Err::Failure(e) => Err(e),
            Err::Error(e) => Err(e),
            _ => Err(ParseError::default()),
        },
    }
}

#[test]
fn test_parse_source_error() {
    // let input = "asdf";
    // let result = parse_source(input);

    // assert!(result.is_err(), "Result of parsing 'asdf' is an error.");

    let input = "100 + ";
    let result = parse_source(input);
    assert!(result.is_err(), "Result of parsing '100 + ' is an error.");
    let err = result.unwrap_err();
    println!("{:#?}", err);
    println!("{}", error::convert_error(err));
    // assert_eq!(
    //     format!("{:#?}", err),
    //     "ASDF",
    //     "ParseError has correct debug string"
    // );
    // assert_eq!(
    //     format!("{}", err),
    //     "ASDF",
    //     "ParseError has correct display string"
    // );
}

fn parse_statement(input: InputType) -> IResult<Expression> {
    let t = nom::error::context(
        "parsing statement",
        nom::Parser::and(
            expr::parse_expression,
            nom::branch::alt((nom::character::complete::line_ending, nom::combinator::eof)),
        ),
    )(input);
    t.map(|(input, tuple)| (input, tuple.0))
}
