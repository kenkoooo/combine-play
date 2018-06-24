extern crate combine;

use combine::error::{Consumed, ConsumedResult, ParseError, Tracked};
use combine::lib::marker::PhantomData;
use combine::{Parser, Stream, StreamOnce};

use combine::parser::char::{char, digit, spaces, string};
use combine::parser::choice::{choice, optional};
use combine::parser::function::parser;
use combine::parser::item::{any, satisfy, satisfy_map};
use combine::parser::repeat::{many, many1, sep_by};
use combine::parser::sequence::between;

#[derive(PartialEq, Debug)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Object(Vec<(String, Value)>),
}

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn integer<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(many1(digit()))
        .map(|s: String| {
            let mut n = 0;
            for c in s.chars() {
                n = n * 10 + (c as i64 - '0' as i64);
            }
            n
        })
        .expected("integer")
}

fn number<I>() -> impl Parser<Input = I, Output = f64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let i = char('0').map(|_| 0.0).or(integer().map(|x| x as f64));
    let fractional = many(digit()).map(|digits: String| {
        let mut magnitude = 1.0;
        digits.chars().fold(0.0, |acc, d| {
            magnitude /= 10.0;
            match d.to_digit(10) {
                Some(d) => acc + (d as f64) * magnitude,
                None => panic!("Not a digit"),
            }
        })
    });

    let exp = satisfy(|c| c == 'e' || c == 'E').with(optional(char('-')).and(integer()));
    lex(optional(char('-'))
        .and(i)
        .map(|(sign, n)| if sign.is_some() { -n } else { n })
        .and(optional(char('.')).with(fractional))
        .map(|(x, y)| if x >= 0.0 { x + y } else { x - y })
        .and(optional(exp))
        .map(|(n, exp_option)| match exp_option {
            Some((sign, e)) => {
                let e = if sign.is_some() { -e } else { e };
                n * 10.0f64.powi(e as i32)
            }
            None => n,
        })).expected("number")
}

fn json_char<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (c, consumed) = try!(any().parse_lazy(input).into());
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });
        match c {
            '\\' => consumed.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, consumed)),
        }
    })
}

fn json_string<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(lex(char('"')), lex(char('"')), many(json_char())).expected("string")
}

fn salmon_key<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(many(parser(|input: &mut I| {
        let (c, consumed) = try!(any().parse_lazy(input).into());
        if c.is_alphanumeric() || c == '_' {
            Ok((c, consumed))
        } else {
            Err(Consumed::Empty(I::Error::empty(input.position()).into()))
        }
    })))
}

fn object<I>() -> impl Parser<Input = I, Output = Value>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let field = (
        optional(salmon_key()),
        optional(lex(string(":="))),
        json_value(),
    ).map(|(key, _, value)| match key {
        Some(key) => (key, value),
        None => ("".to_owned(), value),
    });
    let fields = sep_by(field, lex(char(',')));
    between(lex(char('[')), lex(char(']')), fields)
        .map(Value::Object)
        .expected("object")
}

#[inline(always)]
fn json_value<I>() -> impl Parser<Input = I, Output = Value>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    JsonValueParser(PhantomData)
}

struct JsonValueParser<I>(PhantomData<fn(I) -> Value>);

impl<I> Parser for JsonValueParser<I>
where
    <I as StreamOnce>::Error:
        ParseError<<I as StreamOnce>::Item, <I as StreamOnce>::Range, <I as StreamOnce>::Position>,
    I: Stream<Item = char>,
{
    type Input = I;
    type Output = Value;
    type PartialState = ();

    #[inline]
    fn parse_partial(
        &mut self,
        input: &mut Self::Input,
        _: &mut Self::PartialState,
    ) -> ConsumedResult<Value, I> {
        let ref mut state = Default::default();
        {
            choice((
                json_string().map(Value::String),
                object(),
                number().map(Value::Number),
                lex(string("false").map(|_| Value::Bool(false))),
                lex(string("true").map(|_| Value::Bool(true))),
                lex(string("null").map(|_| Value::Null)),
            ))
        }.parse_partial(input, state)
    }

    #[inline]
    fn add_error(&mut self, errors: &mut Tracked<<I as StreamOnce>::Error>) {
        let mut parser = {
            choice((
                json_string().map(Value::String),
                object(),
                number().map(Value::Number),
                lex(string("false").map(|_| Value::Bool(false))),
                lex(string("true").map(|_| Value::Bool(true))),
                lex(string("null").map(|_| Value::Null)),
            ))
        };
        {
            let _: &mut Parser<Input = I, Output = Value, PartialState = _> = &mut parser;
        }
        parser.add_error(errors)
    }
}

fn main() {
    let input = r#"[
        object := [],
        number := 3.14,
        small_number := 0.59,
        int := -100,
        exp := -1e2,
        exp_neg := 23e-2,
        true := true,
        false   := false,
        null  := null,
        salmon_key1  := true

    ]"#;
    let result = json_value().easy_parse(input);
    println!("{:?}", result);
}
