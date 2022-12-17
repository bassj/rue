use nom::{
    error::ParseError,
    IResult, Parser
};


pub fn ws<'p, O, E: ParseError<&'p [u8]>, F>(
    f: F,
) -> impl FnMut(&'p [u8]) -> IResult<&'p [u8], O, E>
where
    F: Parser<&'p [u8], O, E>,
{
    nom::sequence::delimited(
        nom::character::complete::multispace0,
        f,
        nom::character::complete::multispace0,
    )
}

#[test]
fn test_ws() {
    use crate::ast::Operator;

    let parser = super::atom::build_operator_parser(None);

    let (input, operator) = parser(b"    -").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Subtract,
        "Parser returned correct operator type"
    );

    let (input, operator) = parser(b"    -     ").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Subtract,
        "Parser returned correct operator type"
    );

    let (input, operator) = parser(b"       -         ").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        Operator::Subtract,
        "Parser returned correct operator type"
    );
}
