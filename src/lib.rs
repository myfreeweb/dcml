#![recursion_limit="1000"]

extern crate num;
extern crate im;
#[macro_use]
extern crate pest;
#[macro_use]
extern crate error_chain;

pub mod value {
    pub use num::BigRational;
    pub use im::{List, Map};
    use std::fmt;

    #[derive(PartialEq, Eq)]
    pub enum Value {
        Text(String),
        Number(BigRational),
        Boolean(bool),
        Array(List<Value>),
        Dict(Map<String, Value>),
        Tagged(String, Box<Value>),
        Void,
    }

    impl fmt::Debug for Value {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match self {
                &Value::Text(ref x) => write!(f, "\"{}\"", x),
                &Value::Number(ref x) => write!(f, "{}", x),
                &Value::Boolean(x) => write!(f, "{}", x),
                &Value::Array(ref x) => {
                    write!(f, "[ ")?;
                    for y in x {
                        write!(f, "{:?}; ", y)?;
                    }
                    write!(f, "]")
                },
                &Value::Dict(ref x) => {
                    write!(f, "{{ ")?;
                    for (k, v) in x {
                        write!(f, "{}: {:?}; ", k, v)?;
                    }
                    write!(f, "}}")

                },
                &Value::Tagged(ref t, ref x) => write!(f, "(#{} {:?})", t, x),
                &Value::Void => write!(f, "()"),
            }
        }
    }
}

pub mod errors {
    error_chain! {
        errors { FloatToRationalError }
        foreign_links {
            BigInt(::num::bigint::ParseBigIntError);
            Float(::std::num::ParseFloatError);
        }
    }
}

use pest::prelude::*;
use num::*;
use value::*;
use errors::*;

impl_rdp! {
    grammar! {
        expression = _{
            //{ void | ["("] ~ expression ~ [")"] | tagged | array | dict | vardecl | number | text | interptext }
            { void | ["("] ~ expression ~ [")"] | tagged | array | number }
            //addition       = { plus  | minus }
            //multiplication = { times | slash | percent }
        }
        void = { ["()"] }
        tagged = {< tag ~ expression }

        number = _{ hex | bin | float | dec }
        hex    = @{ ["-"]? ~ ["0x"] ~ hexdigit+ }
        hexdigit  = _{ ['0'..'9'] | ['a'..'f'] | ['A'..'F'] | ["_"] }
        bin    = @{ ["-"]? ~ ["0b"] ~ bindigit+ }
        bindigit = _{ ['0'..'1'] | ["_"] }
        float  = @{ ["-"]? ~ decdigit* ~ ["."] ~ decdigit+ }
        dec    = @{ ["-"]? ~ decdigit+ }
        decdigit = _{ ['0'..'9'] | ["_"] }

        // text    = @{ ["\""] ~ (escape | !(["\""] | ["\\"]) ~ any)* ~ ["\""] }
        // interptext = @{ ["`"] ~ (escape | !(["`"] | ["\\"]) ~ any)* ~ ["`"] }
        // escape  = { ["\\"] ~ (["\""] | ["\\"] | ["/"] | ["t"] | ["n"] | ["r"] | ["b"] | ["f"] | uchar) }
        // uchar   = { ["u"] ~ hexdigit ~ hexdigit ~ hexdigit ~ hexdigit }

        array = { ["["] ~ arritem* ~ ["]"] }
        arritem = { array | expression }

        // dict = { ["{"] ~ pair* ~ ["}"] }
        // pair = { id ~ expression }

        tag = @{ ["#"] ~ id }

        //vardecl = { ["let"] ~ id ~ ["="] ~ expression }

        id = @{ (['a'..'z'] | ['A'..'Z'] | ['0'..'9'])+ }

        // plus   = { ["+"] }
        // minus  = { ["-"] }
        // times  = { ["*"] }
        // slash  = { ["/"] }
        // percent = { ["%"] }

        whitespace = _{ [" "] | ["\t"] | ["\n"] | ["\r"] | ["\u{000C}"] }
        comment = _{ ["//"] ~ (!(["\n"] | ["\r"]) ~ any)* ~ (["\n"] | ["\r\n"] | ["\r"] | eoi) }
    }

    process! {
        eval(&self) -> Result<Value> {
            (_: void) => { Ok(Value::Void) },

            (&d: dec) => { Ok(Value::Number(BigRational::from_integer(BigInt::from_str_radix(&d.replace("_", ""), 10)?))) },
            (&h: hex) => { Ok(Value::Number(BigRational::from_integer(BigInt::from_str_radix(&h.replace("_", "").replace("0x", ""), 16)?))) },
            (&b: bin) => { Ok(Value::Number(BigRational::from_integer(BigInt::from_str_radix(&b.replace("_", "").replace("0b", ""), 2)?))) },
            (&f: float) => { Ok(Value::Number(BigRational::from_float(f.replace("_", "").parse::<f64>()?).ok_or(ErrorKind::FloatToRationalError)?)) },

            (_: tagged, _: tag, &t: id, e: eval()) => { Ok(Value::Tagged(t.to_owned(), Box::new(e?))) },

            (_: array, a: _array()) => { Ok(Value::Array(a?)) },
        }

        _array(&self) -> Result<List<Value>> {
            (_: arritem, head: eval(), tail: _array()) => { Ok(tail?.push_front(head?)) },
            () => { Ok(List::new()) },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    //use num::*;
    use pest::StringInput;

    fn eval(x: &str) -> Value {
        let mut parser = Rdp::new(StringInput::new(x));
        assert!(parser.expression());
        parser.eval().unwrap()
    }

    fn int(x: &[u8], radix: u32) -> Value {
        Value::Number(BigRational::from_integer(BigInt::parse_bytes(x, radix).unwrap()))
    }

    fn rat(numer: &[u8], denom: &[u8], radix: u32) -> Value {
        Value::Number(BigRational::new(BigInt::parse_bytes(numer, radix).unwrap(), BigInt::parse_bytes(denom, radix).unwrap()))
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
    fn test_tagged() {
        assert_eq!(Value::Tagged("testTag".to_owned(), Box::new(Value::Void)), eval("#testTag ()"));
    }

    #[test]
    fn test_array() {
        assert_eq!(Value::Array(List::new()), eval("[]"));
        assert_eq!(arr(vec![int(b"1", 10)]), eval("[1]"));
        assert_eq!(arr(vec![int(b"1", 10), int(b"2", 10)]), eval("[1 2]"));
        //assert_eq!(arr(vec![Value::Tagged("x".to_owned(), Box::new(int(b"1", 10))), arr(vec![int(b"123", 10)]), int(b"2", 10), Value::Void, int(b"3", 10)]), eval("[	#x 1 [\n123\r\n] 2 		() 3]"));
        // XXX: nested arrays are broken
    }

}
