use crate::ast::Expression;

use super::{InputType, IResult};

fn parse_function_name(input: InputType) -> IResult<String> {
    let (input, (first_part, second_part)) = nom::sequence::tuple((
        nom::bytes::complete::take_while1(|c: char| c.is_alphabetic() || c == '_'),
        nom::bytes::complete::take_while(|c: char| c.is_alphanumeric() || c == '_')
    ))(input)?;

    let mut function_name = first_part.to_string();

    function_name.extend(second_part.chars());

    Ok((input, function_name))
}

#[test]
fn test_parse_function_name() {
    use nom_locate::LocatedSpan;

    let input = LocatedSpan::new("test_function()");
    let (input, func_name) = parse_function_name(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"()",
        "Correctly parsed the full function name"
    );

    assert_eq!(
        &func_name,
        &"test_function",
        "Parser returned the correct function name"
    );

    let input = LocatedSpan::new("test_function1()");
    let (input, func_name) = parse_function_name(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"()",
        "Correctly parsed the full function name"
    );

    assert_eq!(
        &func_name,
        &"test_function1",
        "Parser returned the correct function name"
    );


    let input = LocatedSpan::new("test1_function()");
    let (input, func_name) = parse_function_name(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"()",
        "Correctly parsed the full function name"
    );

    assert_eq!(
        &func_name,
        &"test1_function",
        "Parser returned the correct function name"
    );


    let input = LocatedSpan::new("_test()");
    let (input, func_name) = parse_function_name(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"()",
        "Correctly parsed the full function name"
    );

    assert_eq!(
        &func_name,
        &"_test",
        "Parser returned the correct function name"
    );
}

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

    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned correct input"
    );

    assert_eq!(
        func_args,
        Vec::new(),
        "Parser returned the correct function arguments"
    );

    let input = LocatedSpan::new("( 1 , 2 , 3 , 4 )");
    let (input, func_args) = parse_function_arguments(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned correct input"
    );

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
        nom::sequence::tuple((parse_function_name, parse_function_arguments)),
        |(func_name, func_args)| Expression::FunctionInvocation(func_name, func_args),
    )(input)
}

#[test]
pub fn test_parse_function_invocation() {
    use nom_locate::LocatedSpan;    

    let input = LocatedSpan::new("test_function()");
    let (input, func_invoc) = parse_function_invocation(input).unwrap();

    assert_eq!(
        input.fragment(),
        &"",
        "Parser returned the correct input"
    );

    assert_eq!(
        func_invoc,
        Expression::FunctionInvocation(String::from("test_function"), vec![]),
        "Parser returns the correct function invocation"
    );
}