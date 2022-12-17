use nom::error::VerboseError;

use super::InputType;

#[derive(Debug)]
pub struct ParseError;

type NomErr<'p> = nom::Err<VerboseError<InputType<'p>>>;

pub fn build_parse_error(err: NomErr) -> ParseError {
    println!("Got parse error: {:?}", err);

    ParseError
}
