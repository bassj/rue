use crate::ast::Expression;

use super::{IResult, InputType, util};

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

pub fn parse_function_invocation(input: InputType) -> IResult<Expression> {
    nom::combinator::map(
        nom::sequence::tuple((util::parse_identifier, parse_function_arguments)),
        |(func_name, func_args)| Expression::FunctionInvocation(func_name, func_args),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::RueInteger;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_parse_function_arguments() {

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
                Expression::Literal(RueInteger::from(1).into()),
                Expression::Literal(RueInteger::from(2).into()),
                Expression::Literal(RueInteger::from(3).into()),
                Expression::Literal(RueInteger::from(4).into()),
            ],
            "Parser returned the correct function arguments"
        );
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
}
