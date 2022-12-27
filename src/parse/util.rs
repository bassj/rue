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

pub fn delimited_preserve_errors<I, O1, O2, O3, E: ParseError<I>, F, G, H>(
    mut first: F,
    mut second: G,
    mut third: H,
) -> impl FnMut(I) -> IResult<I, (O2, Vec<E>), E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, (O2, Vec<E>), E>,
    H: Parser<I, O3, E>,
{
    move |input: I| {
        let (input, _) = first.parse(input)?;
        let (input, (o2, error_stack)) = second.parse(input)?;
        match third.parse(input) {
            Ok((i, _)) => Ok((i, (o2, error_stack))),
            Err(e) => match e {
                nom::Err::Error(mut e) => {
                    for stack_err in error_stack {
                        e = e.or(stack_err);
                    }
                    Err(nom::Err::Error(e))
                }
                nom::Err::Failure(mut e) => {
                    for stack_err in error_stack {
                        e = e.or(stack_err);
                    }
                    Err(nom::Err::Failure(e))
                }
                _ => unimplemented!(),
            },
        }
    }
}
