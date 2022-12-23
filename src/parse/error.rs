use nom::{Err, IResult};

use super::InputType;

pub trait Expectable {
    fn expectation_name() -> &'static str;
}

#[derive(Debug)]
pub enum Expectation {
    Eof,
    Custom(&'static str),
}

impl std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Expectation::Eof => write!(f, "eof"),
            Expectation::Custom(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
pub enum ErrorContext {
    Expectation(Expectation),
    Kind(nom::error::ErrorKind),
    Context(&'static str),
}

#[derive(Debug)]
pub enum ParseError<'s> {
    Root {
        location: &'s str,
        kind: ErrorContext,
    },
    Stack {
        root: Box<Self>,
        context: Vec<(&'s str, ErrorContext)>,
    },
}

impl<'s> ParseError<'s> {
    pub fn location(&self) -> &'s str {
        match &self {
            &ParseError::Root { location, kind } => location,
            &ParseError::Stack { root, context } => root.location(),
        }
    }
}

impl<'s> nom::error::ParseError<InputType<'s>> for ParseError<'s> {
    fn from_error_kind(input: InputType<'s>, kind: nom::error::ErrorKind) -> Self {
        let input = std::str::from_utf8(input).unwrap();

        Self::Root {
            location: input,
            kind: ErrorContext::Kind(kind),
        }
    }

    fn append(input: InputType<'s>, kind: nom::error::ErrorKind, other: Self) -> Self {
        let input = std::str::from_utf8(input).unwrap();

        let ctx = (input, ErrorContext::Kind(kind));

        match other {
            Self::Stack { root, mut context } => Self::Stack {
                root,
                context: {
                    context.push(ctx);
                    context
                },
            },
            root => Self::Stack {
                root: Box::new(root),
                context: vec![ctx],
            },
        }
    }
}

impl<'s> nom::error::ContextError<InputType<'s>> for ParseError<'s> {
    fn add_context(input: InputType<'s>, ctx: &'static str, mut other: Self) -> Self {
        let input = std::str::from_utf8(input).unwrap();

        let ctx = (input, ErrorContext::Context(ctx));

        match other {
            Self::Stack { root, mut context } => Self::Stack {
                root,
                context: {
                    context.push(ctx);
                    context
                },
            },
            root => Self::Stack {
                root: Box::new(root),
                context: vec![ctx],
            },
        }
    }
}

impl<'s> nom::error::FromExternalError<InputType<'s>, Box<dyn std::error::Error>>
    for ParseError<'s>
{
    fn from_external_error(
        input: InputType<'s>,
        kind: nom::error::ErrorKind,
        e: Box<dyn std::error::Error>,
    ) -> Self {
        let input = std::str::from_utf8(input).unwrap();
        Self::Root {
            location: input,
            kind: ErrorContext::Kind(kind),
        }
    }
}

impl<'s> Default for ParseError<'s> {
    fn default() -> Self {
        Self::Root {
            location: "",
            kind: ErrorContext::Kind(nom::error::ErrorKind::Fail),
        }
    }
}

pub fn convert_error(err: ParseError) -> String {
    todo!();
}

pub fn expect<'s, I, O, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O, ParseError<'s>>
where
    O: Expectable,
    F: nom::Parser<I, O, ParseError<'s>>,
{
    move |input: I| match parser.parse(input) {
        Err(err) => {
            let err: ParseError = match err {
                Err::Error(e) => e,
                Err::Failure(e) => e,
                _ => panic!("Unexpected, should not be incomplete"),
            };

            let kind = ErrorContext::Expectation(Expectation::Custom(O::expectation_name()));

            Err(Err::Failure(ParseError::Root {
                location: err.location(),
                kind,
            }))
        }
        rest => rest,
    }
}

impl Expectable for crate::ast::Expression {
    fn expectation_name() -> &'static str {
        "expression"
    }
}