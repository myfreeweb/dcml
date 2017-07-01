use std;
use std::borrow::Cow;
use std::char;
use num::*;
use num::bigint::ParseBigIntError;
use combine::Parser;
use combine::primitives::{ParseResult, Stream};
use combine::combinator::{between, many, many1, optional, try, satisfy, parser};
use combine::char::{alpha_num, digit, hex_digit, spaces, char, string};
use combine_language::{expression_parser, Assoc, Fixity};
use value::*;

#[inline]
fn bigint(sgn: Option<char>, digits: String, radix: u32) -> Result<BigInt, ParseBigIntError> {
    BigInt::from_str_radix(
        &(sgn.map(|x: char| x.to_string()).unwrap_or_else(
            || "".to_string(),
        ) + &digits.replace("_", "")),
        radix,
    )
}

#[inline]
fn parse_bigint(sgn: Option<char>, digits: String, radix: u32) -> Value {
    if let Ok(bint) = bigint(sgn, digits, radix) {
        Value::Number(BigRational::from_integer(bint))
    } else {
        if cfg!(test) {
            panic!("BigInt parse");
        } else {
            Value::Void
        }
    }
}

#[inline]
fn float(sgn: Option<char>, l: Option<String>, r: String) -> Result<f64, std::num::ParseFloatError> {
    (sgn.map(|x: char| x.to_string()).unwrap_or_else(
        || "".to_string(),
    ) + &l.unwrap_or_else(|| "0".to_string()) + "." + &r)
        .replace("_", "")
        .parse::<f64>()
}

#[inline]
fn parse_float(sgn: Option<char>, l: Option<String>, r: String) -> Value {
    if let Ok(f) = float(sgn, l, r) {
        if let Some(br) = BigRational::from_float(f) {
            Value::Number(br)
        } else {
            if cfg!(test) {
                panic!("BigRational from float");
            } else {
                Value::Void
            }
        }
    } else {
        if cfg!(test) {
            panic!("float parse");
        } else {
            Value::Void
        }
    }
}

fn apply_op(l: Value, op: &'static str, r: Value) -> Value {
    match (l, r) {
        (Value::Number(ln), Value::Number(rn)) => {
            match op {
                "+" => Value::Number(ln + rn),
                "-" => Value::Number(ln - rn),
                "*" => Value::Number(ln * rn),
                "/" => Value::Number(ln / rn),
                "%" => Value::Number(ln % rn),
                _ => unreachable!(),
            }
        },
        (Value::Text(lv), Value::Text(rv)) => {
            match op {
                "+" => Value::Text(lv + &rv),
                _ => Value::Void,
            }
        },
        (Value::Array(lv), Value::Array(rv)) => {
            match op {
                "+" => Value::Array(lv + rv),
                _ => Value::Void,
            }
        },
        (Value::Dict(lv), Value::Dict(rv)) => {
            match op {
                "+" => Value::Dict(lv.union(&rv)),
                _ => Value::Void,
            }
        },
        (Value::Tagged(t, lv), rv) => Value::Tagged(t, Box::new(apply_op(*lv, op, rv))),
        (lv, Value::Tagged(t, rv)) => Value::Tagged(t, Box::new(apply_op(lv, op, *rv))),
        _ => Value::Void,
    }
}

macro_rules! or {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+) => { $head$(.or($tail))* };
}

macro_rules! chars {
    ($($ch:expr),+) => { or![$(char($ch)),*] };
}

macro_rules! strings {
    ($($str:expr),+) => { or![$(string($str)),*] };
}

pub fn eval<I>(input: I) -> ParseResult<Value, I>
    where I: Stream<Item = char>
{
    let recur = || parser(eval::<I>);
    let lex_char = |c| char(c).skip(spaces());
    let ident = || many1(alpha_num());

    let void = string("()").map(|_| Value::Void);

    let signum = || optional(chars!['-', '+']);
    let decimal = signum().then(|sgn| many1(digit().or(char('_'))).map(move |digits| parse_bigint(sgn, digits, 10)));
    let hexadecimal = signum().and(string("0x")).then(|(sgn, _)| {
            many1(hex_digit().or(char('_'))).map(move |digits| parse_bigint(sgn, digits, 16))
        });
    let binary = signum().and(string("0b")).then(|(sgn, _)| {
            many1(chars!['_', '0', '1']).map(move |digits| parse_bigint(sgn, digits, 2))
        });
    let float = signum().then(|sgn| {
        optional(many1(digit().or(char('_'))))
            .skip(char('.'))
            .and(many1(digit().or(char('_'))))
            .map(move |(l, r)| parse_float(sgn, l, r))
    });

    let escape = || {
        char('\\')
            .and(or![char('"').map(|_| '\"'),
                char('\\').map(|_| '\\'),
                char('/').map(|_| '/'),
                char('$').map(|_| '$'),
                char('b').map(|_| '\x08'),
                char('f').map(|_| '\x0C'),
                char('n').map(|_| '\n'),
                char('r').map(|_| '\r'),
                char('t').map(|_| '\t'),
                (char('u'), many1(hex_digit())).map(|(_, ds): (_, String)| {
                char::from_u32(u32::from_str_radix(&ds, 16).unwrap_or(0xFFFD)).unwrap_or('�')
            })])
            .map(|(_, x)| x)
    };
    let quoted_string_bare = || between(char('"'), char('"'), many(escape().or(satisfy(|c| c != '"'))));
    let quoted_string = quoted_string_bare().map(Value::Text);
    let interpolation = || {
        char('$')
            .skip(char('{'))
            .and(recur())
            .skip(char('}'))
            .map(|(_, x)| format!("{}", x))
    };
    let interpolated_string_bare = || {
        between(
            char('`'),
            char('`'),
            many(
                escape()
                    .map(|x| format!("{}", x))
                    .or(interpolation())
                    .or(many1(satisfy(|c| c != '`' && c != '\\' && c != '$'))),
            ),
        ).map(|ss: Vec<String>| ss.join(""))
    };
    let interpolated_string = interpolated_string_bare().map(Value::Text);

    let array = between(lex_char('['), lex_char(']'), many(recur())).map(Value::Array);

    let dict = between(
        lex_char('{'),
        lex_char('}'),
        many((or![ident(), quoted_string_bare(), interpolated_string_bare()], recur())),
    ).map(Value::Dict);

    let tagged = char('#').and(ident()).skip(spaces()).then(|(_, id): (_,
                   String)| {
            let id_c = Cow::from(id);
            recur().map(move |x| Value::Tagged(id_c.as_ref().to_owned(), Box::new(x)))
        });

    let parens = between(lex_char('('), lex_char(')'), recur());

    let op = strings!["+", "-", "*", "/", "%"].map(|op| {
        (
            op,
            Assoc {
                precedence: match op {
                    "+" | "-" => 69,
                    _ => 420,
                },
                fixity: Fixity::Left,
            },
        )
    });

    let expr = or![try(void), parens, array, dict, tagged, quoted_string, interpolated_string, try(hexadecimal), try(binary), try(float), decimal];
    let expr_with_spaces = spaces().and(expr).map(|(_, x)| x).skip(spaces());
    expression_parser(expr_with_spaces, op, apply_op).parse_stream(input)
}

#[cfg(test)]
mod tests {
    use num::*;
    use value::*;
    use parser;

    fn eval(x: &str) -> Value {
        let (result, _) = parser::eval(x).unwrap();
        result
    }

    fn int(x: &[u8], radix: u32) -> Value {
        Value::Number(BigRational::from_integer(BigInt::parse_bytes(x, radix).unwrap()))
    }

    fn rat(numer: &[u8], denom: &[u8], radix: u32) -> Value {
        Value::Number(BigRational::new(
            BigInt::parse_bytes(numer, radix).unwrap(),
            BigInt::parse_bytes(denom, radix).unwrap(),
        ))
    }

    fn arr(x: Vec<Value>) -> Value {
        Value::Array(List::from(x))
    }

    #[test]
    fn test_void() {
        assert_eq!(Value::Void, eval("()"));
    }

    #[test]
    fn test_dec() {
        assert_eq!(Value::Number(Zero::zero()), eval("0"));
        assert_eq!(Value::Number(Zero::zero()), eval("-0"));
        assert_eq!(int(b"-123", 10), eval("-123"));
        assert_eq!(int(b"123", 10), eval("123"));
        assert_eq!(int(b"123", 10), eval("1_2_3___"));
    }

    #[test]
    fn test_hex() {
        assert_eq!(Value::Number(Zero::zero()), eval("0x000"));
        assert_eq!(Value::Number(Zero::zero()), eval("-0x000"));
        assert_eq!(int(b"-deadbeef", 16), eval("-0xDeADbeEF"));
        assert_eq!(int(b"ff", 16), eval("0xFf"));
    }

    #[test]
    fn test_bin() {
        assert_eq!(Value::Number(Zero::zero()), eval("0b0"));
        assert_eq!(Value::Number(Zero::zero()), eval("-0b0"));
        assert_eq!(int(b"-0101", 2), eval("-0b0101"));
        assert_eq!(int(b"1010", 2), eval("0b1010"));
    }

    #[test]
    fn test_float() {
        assert_eq!(Value::Number(Zero::zero()), eval("0.0"));
        assert_eq!(Value::Number(Zero::zero()), eval("-0.0"));
        assert_eq!(rat(b"-1", b"2", 10), eval("-0.5"));
        assert_eq!(rat(b"1", b"2", 10), eval("0.5"));
        assert_eq!(rat(b"1", b"2", 10), eval("._5"));
    }

    #[test]
    fn test_arith() {
        assert_eq!(rat(b"-75", b"2", 10), eval(" 2.5* -0xF	"));
        assert_eq!(int(b"6", 10), eval("420%69"));
        assert_eq!(int(b"6", 10), eval("\n(1 + 2) *2"));
        assert_eq!(int(b"5", 10), eval("\n1+ 2 \r*\n	2"));
        assert_eq!(Value::Tagged("test".to_owned(), Box::new(int(b"5", 10))), eval("1+ #test 2*2"));
    }

    #[test]
    fn test_text() {
        assert_eq!(Value::Text("".to_owned()), eval(r#" "" "#));
        assert_eq!(Value::Text("\n	💯:)".to_owned()), eval(r#" "\n\t\u1F4AF\u003a\u0029" "#));
        assert_eq!(Value::Text("hello\nworld\n!!".to_owned()),
            eval(
                r#" "hello
world\n!!" "#,
            ));
        assert_eq!(Value::Text("hello 2 world".to_owned()), eval(r#" `hello ${1 + 1} world` "#));
        assert_eq!(Value::Text("some\r\ntext$".to_owned()), eval(r#" `some${ 	`${"\r"}\n` }text\$` "#));
    }

    #[test]
    fn test_text_plus() {
        assert_eq!(Value::Text("dank  memes".to_owned()), eval(r#" ""+"dank" +	" " +  " " + "memes" +"" "#));
    }

    #[test]
    fn test_tagged() {
        assert_eq!(Value::Tagged("testTag".to_owned(), Box::new(Value::Void)), eval("#testTag ()"));
    }

    #[test]
    fn test_array() {
        assert_eq!(Value::Array(List::new()), eval("[]"));
        assert_eq!(arr(vec![int(b"1", 10)]), eval("[1]"));
        assert_eq!(arr(vec![int(b"1", 10), int(b"2", 10)]), eval("[1 2]"));
        assert_eq!(
            arr(vec![Value::Tagged("x".to_owned(), Box::new(int(b"0", 10))),
                 arr(vec![int(b"123", 10)]),
                 int(b"2", 10),
                 Value::Void,
                 int(b"3", 10)]),
            eval(" [	#x 0 [\n123\r\n] 2 		() 3]")
        );
    }

    #[test]
    fn test_array_plus() {
        assert_eq!(arr(vec![int(b"1", 10), int(b"2", 10)]), eval("[1] + [] + [2]"));
    }

    #[test]
    fn test_dict() {
        assert_eq!(Value::Dict(Map::new()), eval("{}"));
        assert_eq!(Value::Dict(map!{ "thing".to_owned() => int(b"1", 10) }), eval("{ thing 1 }"));
        assert_eq!(
            Value::Dict(map!{
                "thing".to_owned() => arr(vec![int(b"1", 10)]),
                "otherThing".to_owned() => Value::Tagged("test".to_owned(), Box::new(Value::Dict(map!{})))
            }),
            eval("{ thing \r\n[1] 	otherThing #test { } }")
        );
        assert_eq!(
            Value::Dict(
                map!{ "twotimes2".to_owned() => int(b"4", 10), "my thing".to_owned() => Value::Text("something".to_owned()) },
            ),
            eval("{ `twotimes${1 + 1}` (1 + 1) * 2 \"my thing\" \"some\" + \"thing\" }")
        );
    }

    #[test]
    fn test_dict_plus() {
        assert_eq!(
            Value::Dict(map!{
                "thing".to_owned() => int(b"1", 10),
                "other".to_owned() => int(b"2", 10)
            }),
            eval("{ thing 1 } + { other 2 } + {}")
        );
    }

}
