use nom::IResult;

pub mod types;

/// The main entry point for parsing a rue program
pub fn parse_source(input: &str) -> types::Node {
    let (_, expr) = parse_expression(input.as_bytes()).expect("Error parsing input");
    expr
}

/// Parses an expression from the input
fn parse_expression(input: &[u8]) -> IResult<&[u8], types::Expression> {
    parse_binary_operation(input)
}

#[test]
fn test_parse_expression_nested_expression() {
    // let (input, expr) = parse_binary_operation(b"(100+201)").unwrap();
    // assert_eq!(
    //     expr,
    //     types::Expression::BinaryOperation(
    //         types::Operator::Add,
    //         Box::new(types::Expression::IntegerLiteral(100)),
    //         Box::new(types::Expression::IntegerLiteral(201))
    //     )
    // );
    // assert_eq!(input, b"");
}

fn parse_nested_expression(input: &[u8]) -> IResult<&[u8], types::Expression> {
    nom::sequence::delimited(
        nom::character::complete::char('('),
        parse_expression,
        nom::character::complete::char(')'),
    )(input)
}

fn _build_binary_operation_parser<'p>(
    precedence_level: usize,
    precedence_levels: &'static Vec<Vec<types::Operator>>,
) -> impl Fn(&[u8]) -> IResult<&[u8], types::Expression> + 'p {
    let num_precedence_levels = precedence_levels.len();

    move |input: &[u8]| -> IResult<&[u8], types::Expression> {
        let (next_level, operators): (
            Box<dyn Fn(&[u8]) -> IResult<&[u8], types::Expression>>,
            Option<Vec<types::Operator>>,
        ) = if precedence_level < num_precedence_levels {
            let operators = precedence_levels[precedence_level].clone();
            (
                Box::new(_build_binary_operation_parser(
                    precedence_level + 1,
                    precedence_levels,
                )),
                Some(operators),
            )
        } else {
            (Box::new(parse_integer_literal), None)
        };

        if operators.is_none() {
            return next_level(input);
        }

        let parse_op_chain = nom::multi::many0(nom::sequence::tuple((
            build_operator_parser(operators.as_ref()),
            next_level.as_ref(),
        )));

        let res = nom::combinator::map(
            nom::sequence::tuple((next_level.as_ref(), parse_op_chain)),
            move |(mut lhs, op_chain)| {
                if op_chain.len() == 0 {
                    lhs
                } else {
                    for (op, rhs) in op_chain {
                        let _lhs = std::mem::take(&mut lhs);
                        let mut expr =
                            types::Expression::BinaryOperation(op, Box::new(_lhs), Box::new(rhs));
                        std::mem::swap(&mut lhs, &mut expr);
                    }

                    lhs
                }
            },
        )(input);

        res
    }
}

fn parse_binary_operation(input: &[u8]) -> IResult<&[u8], types::Expression> {
    let precedence_levels = types::Operator::precedence_levels();

    _build_binary_operation_parser(0, precedence_levels)(input)
}

#[test]
fn test_parse_binary_operation() {
    // Test parsing simple addition
    let (input, expr) = parse_binary_operation(b"100+201").unwrap();
    assert_eq!(
        expr,
        types::Expression::BinaryOperation(
            types::Operator::Add,
            Box::new(types::Expression::IntegerLiteral(100)),
            Box::new(types::Expression::IntegerLiteral(201))
        )
    );
    assert_eq!(input, b"");
    // Test with three terms
    let (input, expr) = parse_binary_operation(b"100+200+300").unwrap();
    assert_eq!(
        expr,
        types::Expression::BinaryOperation(
            types::Operator::Add,
            Box::new(types::Expression::BinaryOperation(
                types::Operator::Add,
                Box::new(types::Expression::IntegerLiteral(100)),
                Box::new(types::Expression::IntegerLiteral(200))
            )),
            Box::new(types::Expression::IntegerLiteral(300)),
        )
    );
    assert_eq!(input, b"");
    // Test order of operations, with operators of the same precedence.
    let (input, expr) = parse_binary_operation(b"100-101+200").unwrap();
    assert_eq!(
        expr,
        types::Expression::BinaryOperation(
            types::Operator::Add,
            Box::new(types::Expression::BinaryOperation(
                types::Operator::Subtract,
                Box::new(types::Expression::IntegerLiteral(100)),
                Box::new(types::Expression::IntegerLiteral(101)),
            )),
            Box::new(types::Expression::IntegerLiteral(200)),
        )
    );
    assert_eq!(input, b"");
    // Test order of operations with operators of different precedence
    let (input, expr) = parse_binary_operation(b"100/2+300/4").unwrap();
    assert_eq!(
        expr,
        types::Expression::BinaryOperation(
            types::Operator::Add,
            Box::new(types::Expression::BinaryOperation(
                types::Operator::Divide,
                Box::new(types::Expression::IntegerLiteral(100)),
                Box::new(types::Expression::IntegerLiteral(2))
            )),
            Box::new(types::Expression::BinaryOperation(
                types::Operator::Divide,
                Box::new(types::Expression::IntegerLiteral(300)),
                Box::new(types::Expression::IntegerLiteral(4))
            ))
        )
    );
    assert_eq!(input, b"");
}

fn build_operator_parser(
    operators: Option<&Vec<types::Operator>>,
) -> impl Fn(&[u8]) -> IResult<&[u8], types::Operator> {
    let operator_tokens = match operators {
        Some(operators) => operators
            .into_iter()
            .map(|op| op.get_token())
            .collect::<String>(),
        None => types::Operator::tokens(),
    };

    move |input| {
        nom::combinator::map_res(
            nom::character::complete::one_of(operator_tokens.as_str()),
            |c: char| c.try_into() as Result<types::Operator, ()>,
        )(input)
    }
}

#[test]
fn test_build_operator_parser() {
    let addition_parser = build_operator_parser(Some(&vec![types::Operator::Add]));

    let (input, operator) = addition_parser(b"+").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        types::Operator::Add,
        "Parser returned correct operator type"
    );

    let result = addition_parser(b"-");
    assert!(
        result.is_err(),
        "Addition parser returns an error trying to parse '-'"
    );

    let addition_subtraction_parser =
        build_operator_parser(Some(&vec![types::Operator::Add, types::Operator::Subtract]));

    let (input, operator) = addition_parser(b"+").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        types::Operator::Add,
        "Parser returned correct operator type"
    );

    let (input, operator) = addition_subtraction_parser(b"-").unwrap();
    assert_eq!(input, b"", "Parser returned correct input string");
    assert_eq!(
        operator,
        types::Operator::Subtract,
        "Parser returned correct operator type"
    );
}

fn parse_integer_literal(input: &[u8]) -> IResult<&[u8], types::Expression> {
    nom::combinator::map_res(
        nom::bytes::complete::take_while(nom::character::is_digit),
        |value: &[u8]| -> Result<types::Expression, Box<dyn std::error::Error>> {
            let value = std::str::from_utf8(value)?.to_string();
            let value = value.parse::<i64>()?;
            Ok(types::Expression::IntegerLiteral(value))
        },
    )(input)
}

#[test]
fn test_parse_integer_literal() {
    let (input, expr) = parse_integer_literal(b"100").unwrap();
    assert_eq!(
        expr,
        types::Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b"", "Integer literal was removed from input");

    let (input, expr) = parse_integer_literal(b"0").unwrap();
    assert_eq!(
        expr,
        types::Expression::IntegerLiteral(0),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b"", "Integer literal was removed from input");

    let (input, expr) = parse_integer_literal(b"100 200").unwrap();
    assert_eq!(
        expr,
        types::Expression::IntegerLiteral(100),
        "Integer literal was correctly parsed"
    );
    assert_eq!(input, b" 200", "Integer literal was removed from input");

    let res = parse_integer_literal(b"not an int");
    assert!(res.is_err(), "Trying to parse non-operator causes error");

    let res = parse_integer_literal(b"");
    assert!(res.is_err(), "Failing to parse empty string causes error");
}
