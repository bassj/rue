use crate::ast::Expression;

use super::{util::parse_identifier, IResult, InputType};

fn parse_function_arguments(input: InputType) -> IResult<Vec<Expression>> {
    nom::sequence::delimited(
        nom::character::complete::char('('),
        nom::multi::separated_list0(
            nom::character::complete::char(','),
            nom_preserve::error::discard(super::expr::parse_expression),
        ),
        nom::character::complete::char(')'),
    )(input)
}

#[test]
fn test_parse_function_arguments() {
    use nom_locate::LocatedSpan;

    let input = LocatedSpan::new("()");
    let (input, func_args) = parse_function_arguments(input).unwrap();

    assert_eq!(input.fragment(), &"", "Parser returned correct input");

    assert_eq!(
        func_args,
        Vec::new(),
        "Parser returned the correct function arguments"
    );

    let input = LocatedSpan::new("( 1 , 2 , 3 , 4 )");
    let (input, func_args) = parse_function_arguments(input).unwrap();

    assert_eq!(input.fragment(), &"", "Parser returned correct input");

    assert_eq!(
        func_args,
        vec![
            Expression::IntegerLiteral(1),
            Expression::IntegerLiteral(2),
            Expression::IntegerLiteral(3),
            Expression::IntegerLiteral(4),
        ],
        "Parser returned the correct function arguments"
    );
}

pub fn parse_function_invocation(input: InputType) -> IResult<Expression> {
    nom::combinator::map(
        nom::sequence::tuple((parse_identifier, parse_function_arguments)),
        |(func_name, func_args)| Expression::FunctionInvocation(func_name, func_args),
    )(input)
}

#[test]
pub fn test_parse_function_invocation() {
    use nom_locate::LocatedSpan;

    let input = LocatedSpan::new("test_function()");
    let (input, func_invoc) = parse_function_invocation(input).unwrap();

    assert_eq!(input.fragment(), &"", "Parser returned the correct input");

    assert_eq!(
        func_invoc,
        Expression::FunctionInvocation(String::from("test_function"), vec![]),
        "Parser returns the correct function invocation"
    );
}
