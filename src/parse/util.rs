use super::InputType;
use nom::{error::ParseError, IResult, Parser};

pub fn ws<'p, O, E: ParseError<InputType<'p>>, F>(
    f: F,
) -> impl FnMut(InputType<'p>) -> IResult<InputType<'p>, O, E>
where
    F: Parser<InputType<'p>, O, E>,
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
    use nom_locate::LocatedSpan;

    let parser = super::atom::build_operator_parser(None);

    let (input, operator) = parser(LocatedSpan::new("    -")).unwrap();
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

    let (input, operator) = parser(LocatedSpan::new("    -     ")).unwrap();
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

    let (input, operator) = parser(LocatedSpan::new("       -         ")).unwrap();
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