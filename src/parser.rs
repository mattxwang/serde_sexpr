use crate::{needs_quoting, Error, Value};

use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{anychar, char, multispace0},
    combinator::map,
    error::VerboseError,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded},
    IResult, Parser,
};
use std::str::FromStr;

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Value, Error> {
        match parser(s) {
            Ok((rest, value)) => {
                if rest.is_empty() {
                    Ok(value)
                } else {
                    Err(Error::ParseTrailing)
                }
            }
            Err(_) => Err(Error::ParseFailed),
        }
    }
}

fn parser(i: &str) -> IResult<&str, Value, VerboseError<&str>> {
    delimited(multispace0, parse_value, multispace0).parse(i)
}

fn parse_value(i: &str) -> IResult<&str, Value, VerboseError<&str>> {
    alt((parse_list_helper, parse_escaped_symbol, parse_unescaped_symbol)).parse(i)
}

// fn parse_list<'a, O1, F>(inner: F) -> impl Parser<&'a str, O1, VerboseError<&'a str>>
// where
//     F: Parser<&'a str, O1, VerboseError<&'a str>>,
// {
//     delimited(
//         char('('),
//         preceded(multispace0, inner),
//         context("closing paren", cut(preceded(multispace0, char(')')))),
//     )
// }

fn parse_list_helper(i: &str) -> IResult<&str, Value, VerboseError<&str>> {
    // map(parse_list(many0(parse_value)), Value::List).parse(i)
    map(delimited(
        char('('),
        separated_list0(multispace0, parse_value),
        char(')'),
    ), Value::List).parse(i)
}

fn parse_unescaped_symbol(i: &str) -> IResult<&str, Value, VerboseError<&str>> {
    map(take_while1(doesnt_need_quoting), |s: &str| {
        Value::Sym(s.to_string())
    })
    .parse(i)
}

fn parse_escaped_symbol(i: &str) -> IResult<&str, Value, VerboseError<&str>> {
    println!("in escaped s");
    map(
        delimited(char('|'), many0(parse_symbol_chars), char('|')),
        |s: Vec<String>| Value::Sym(s.into_iter().collect()),
    )
    .parse(i)
}

fn parse_symbol_chars(i: &str) -> IResult<&str, String, VerboseError<&str>> {
    alt((parse_escaped_chars, parse_unescaped_chars)).parse(i)
}

fn parse_escaped_chars(i: &str) -> IResult<&str, String, VerboseError<&str>> {
    map(preceded(char('\\'), many1(anychar)), |s| s.iter().collect()).parse(i)
}

fn parse_unescaped_chars(i: &str) -> IResult<&str, String, VerboseError<&str>> {
    println!("in unescaped c");
    map(take_while1(doesnt_need_quoting), String::from).parse(i)
}

// named!(parser<CompleteStr, Value>, do_parse!(ws >> v: value >> ws >> eof!() >> (v)));
// named!(value<CompleteStr, Value>, alt_complete!( list | escaped_sym | unescaped_sym ));

// named!(list<CompleteStr, Value>, map!(delimited!(char!('('), list_body, char!(')')), Value::List));
// named!(list_body<CompleteStr, Vec<Value>>, separated_list!(ws, value));

// named!(escaped_sym<CompleteStr, Value>,
//     map!(delimited!(char!('|'), many0!(sym_chs), char!('|')),
//          |s| Value::Sym(s.into_iter().collect())));
// named!(unescaped_sym<CompleteStr, Value>,
//     map!(take_while1!(doesnt_need_quoting),
//          |s| Value::Sym(s.to_string())));

// named!(sym_chs<CompleteStr, Cow<str>>, alt_complete!(escaped_ch | unescaped_chs));
// named!(escaped_ch<CompleteStr, Cow<str>>, do_parse!(
//     char!('\\') >>
//     ch: anychar >>
//     (Cow::Owned(Some(ch).into_iter().collect()))));
// named!(unescaped_chs<CompleteStr, Cow<str>>,
//     map!(take_while1!(doesnt_need_quoting), |CompleteStr(s)| Cow::Borrowed(s)));

// named!(ws<CompleteStr, ()>, map!(take_while!(char::is_whitespace), |_| ()));

fn doesnt_need_quoting(ch: char) -> bool {
    !needs_quoting(ch)
}
