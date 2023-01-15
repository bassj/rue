use std::{any::Any, collections::HashSet, fmt::Write};

use itertools::Itertools;
use nom::{Err, IResult,  error::ErrorKind};
use nom_locate::LocatedSpan;

use colored::Colorize;

use super::InputType;

pub type ErrorStack<'p> = nom_preserve::error::ErrorStack<InputType<'p>, ParseError<'p>>;

pub trait Expectable {
    fn expectation_name() -> &'static str;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Expectation {
    Eof,
    LineEnding,
    Char(char),
    Custom(&'static str),
    OneOf(Vec<Self>),
}

impl std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectation::Eof => write!(f, "eof"),
            Expectation::LineEnding => write!(f, "newline"),
            Expectation::Custom(name) => write!(f, "{}", name),
            Expectation::Char(c) => write!(f, "`{}`", c),
            Expectation::OneOf(expectations) => {
                let num_unique_expectations = expectations.iter().unique().count();
                let mut expectations_str = String::new();
                let mut seen_patterns: HashSet<&Expectation> = HashSet::new();

                for (index, expectation) in expectations.iter().enumerate() {
                    if seen_patterns.contains(expectation) {
                        continue;
                    }
                    seen_patterns.insert(expectation);

                    let delimeter = if index == 0 {
                        ""
                    } else if index + 1 == num_unique_expectations {
                        " or "
                    } else {
                        ", "
                    };

                    write!(expectations_str, "{}{}", delimeter, expectation)?;
                }

                let predicate = if num_unique_expectations == 1 {
                    ""
                } else if num_unique_expectations == 2 {
                    "either"
                } else {
                    "one of"
                };

                write!(f, "{} {}", predicate, expectations_str)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorContext {
    Expectation { expected: Expectation },
    Kind(nom::error::ErrorKind),
    Context(&'static str),
}

#[derive(Debug, Clone)]
pub enum ParseError<'s> {
    Root {
        location: InputType<'s>,
        kind: ErrorContext,
    },
    Stack {
        root: Box<Self>,
        context: Vec<(InputType<'s>, ErrorContext)>,
    },
    Alt(Vec<Self>),
}

impl<'s> ParseError<'s> {
    pub fn location(&self) -> &InputType<'s> {
        match &self {
            &ParseError::Root { location, kind: _ } => location,
            &ParseError::Stack { root, context: _ } => root.location(),
            &ParseError::Alt(errs) => errs[0].location(),
        }
    }

    pub fn error_message(&self) -> (String, String) {
        fn _match_root(root: &ParseError) -> (String, String) {
            let found = match root.location().fragment().chars().next() {
                Some(c) => format!("`{}`", c),
                _ => "``".to_string(),
            };

            let message = match root {
                ParseError::Root { location: _, kind } => match kind {
                    ErrorContext::Expectation { expected } => format!("expected {}", expected),
                    ErrorContext::Kind(nom_kind) => format!("nom error: {:?}", nom_kind),
                    _ => unimplemented!(),
                },
                ParseError::Alt(errs) => {
                    let expectation = Expectation::OneOf(_extract_expectations(errs));
                    format!("expected {}", expectation)
                }
                _ => unimplemented!(),
            };

            let long_message = format!("{}, found {}", message, found);

            (message, long_message)
        }

        fn _extract_expectations(errs: &Vec<ParseError>) -> Vec<Expectation> {
            let mut expectations = vec![];
            for err in errs {
                match err {
                    ParseError::Root { location: _, kind } => match kind {
                        ErrorContext::Expectation { expected } => expectations.push(expected.clone()),
                        _ => (),
                    },
                    ParseError::Stack { root, context: _ } => match root.as_ref() {
                        ParseError::Root { location: _, kind } => match kind {
                            ErrorContext::Expectation { expected } => expectations.push(expected.clone()),
                            _ => (),
                        }
                        ParseError::Alt(errs) => expectations.extend(_extract_expectations(errs)),
                        _ => panic!("Root shouldn't be anything other than ParseError::Root"),
                    },
                    ParseError::Alt(errs) => {
                        expectations.extend(_extract_expectations(errs))
                    },
                }
            }

            expectations
        }

        match self {
            ParseError::Alt(errs) => {
                let found = match errs[0].location().fragment().chars().next() {
                    Some(c) => format!("`{}`", c),
                    _ => "``".to_string(),
                };

                let expectations = _extract_expectations(errs);
                let short_message = format!("expected {}", expectations[0]);
                let long_message = format!(
                    "expected {}, found {}",
                    Expectation::OneOf(expectations),
                    found
                );

                (short_message, long_message)
            }
            ParseError::Stack { root, context: _ } => _match_root(root.as_ref()),
            root => _match_root(root),
        }
    }

    pub fn error_header(&self) -> String {
        let (_, long_message) = self.error_message();
        format!("{}: {}", "rue error".red(), long_message)
            .bold()
            .white()
            .to_string()
    }

    pub fn code_highlight(&self) -> String {
        let (short_message, _) = self.error_message();

        let location = self.location();
        let line_number = location.location_line().to_string();
        let width = line_number.len() + 1;
        let empty_header = format!("{:w$}|", "", w = width).bold().bright_cyan();
        let line_header = format!("{:w$}|", line_number, w = width)
            .bold()
            .bright_cyan();
        let newline = "\r\n";

        let code = std::str::from_utf8(location.get_line_beginning())
            .expect("Source must be utf8 encoded");
        let code = format!("\t{}", code);

        let offset = location.get_utf8_column();
        let error_highlight = format!("\t{:>offset$} {}", "^", short_message, offset = offset)
            .bold()
            .red();

        format!(
            "{}{}{}{}{}{}{}",
            empty_header, newline, line_header, code, newline, empty_header, error_highlight
        )
    }
}

impl std::fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let newline = "\r\n";
        write!(
            f,
            "{}{}{}",
            self.error_header(),
            newline,
            self.code_highlight()
        )
    }
}

impl<'s> nom::error::ParseError<InputType<'s>> for ParseError<'s> {
    fn from_error_kind(input: InputType<'s>, kind: nom::error::ErrorKind) -> Self {
        Self::Root {
            location: input,
            kind: match kind {
                ErrorKind::Eof => ErrorContext::Expectation { expected: Expectation::Eof },
                ErrorKind::CrLf => ErrorContext::Expectation { expected: Expectation::LineEnding },
                kind => ErrorContext::Kind(kind)
            },
        }
    }

    fn append(input: InputType<'s>, kind: nom::error::ErrorKind, other: Self) -> Self {
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

    fn from_char(input: InputType<'s>, c: char) -> Self {
        Self::Root {
            location: input,
            kind: ErrorContext::Expectation {
                expected: Expectation::Char(c),
            },
        }
    }

    fn or(self, other: Self) -> Self {
        let siblings = match (self, other) {
            (ParseError::Alt(mut siblings1), ParseError::Alt(mut siblings2)) => {
                match siblings1.capacity() >= siblings2.capacity() {
                    true => {
                        siblings1.extend(siblings2);
                        siblings1
                    }
                    false => {
                        siblings2.extend(siblings1);
                        siblings2
                    }
                }
            }
            (ParseError::Alt(mut siblings), err) | (err, ParseError::Alt(mut siblings)) => {
                siblings.push(err);
                siblings
            }
            (err1, err2) => vec![err1, err2],
        };

        ParseError::Alt(siblings)
    }
}

impl<'s> nom::error::ContextError<InputType<'s>> for ParseError<'s> {
    fn add_context(input: InputType<'s>, ctx: &'static str, other: Self) -> Self {
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
        _e: Box<dyn std::error::Error>,
    ) -> Self {
        Self::Root {
            location: input,
            kind: ErrorContext::Kind(kind),
        }
    }
}

impl<'s> Default for ParseError<'s> {
    fn default() -> Self {
        Self::Root {
            location: LocatedSpan::new(""),
            kind: ErrorContext::Kind(nom::error::ErrorKind::Fail),
        }
    }
}

pub fn expect<'s, I, O, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, O, ParseError<'s>>
where
    O: Expectable,
    F: nom::Parser<I, O, ParseError<'s>>,
{
    move |input: I| match parser.parse(input) {
        Err(err) => {
            let kind = ErrorContext::Expectation {
                expected: Expectation::Custom(O::expectation_name()),
            };

            let err = match err {
                Err::Error(e) => Err::Error(ParseError::Root {
                    location: *e.location(),
                    kind,
                }),
                Err::Failure(e) => Err::Failure(ParseError::Root {
                    location: *e.location(),
                    kind,
                }),
                _ => panic!("Unexpected, should not be incomplete"),
            };

            Err(err)
        }
        e => e,
    }
}

impl Expectable for crate::ast::Expression {
    fn expectation_name() -> &'static str {
        "an expression"
    }
}

impl<T> Expectable for (crate::ast::Expression, T)
where
    T: Any,
{
    fn expectation_name() -> &'static str {
        "an expression"
    }
}

impl Expectable for crate::ast::Operator {
    fn expectation_name() -> &'static str {
        "an operator"
    }
}
