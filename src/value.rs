pub use num::{BigInt, BigRational};
pub use im::{List, Map};
use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub enum Value {
    Text(String),
    Number(BigRational),
    Boolean(bool),
    Array(List<Value>),
    Dict(Map<String, Value>),
    Tagged(String, Box<Value>),
    Void,
}

pub fn int(x: &[u8], radix: u32) -> Value {
    Value::Number(BigRational::from_integer(BigInt::parse_bytes(x, radix).unwrap()))
}

pub fn rat(numer: &[u8], denom: &[u8], radix: u32) -> Value {
    Value::Number(BigRational::new(
        BigInt::parse_bytes(numer, radix).unwrap(),
        BigInt::parse_bytes(denom, radix).unwrap(),
    ))
}

pub fn arr(x: Vec<Value>) -> Value {
    Value::Array(List::from(x))
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Value::Text(ref x) => write!(f, "\"{}\"", x),
            Value::Number(ref x) => write!(f, "{}", x),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Array(ref x) => {
                write!(f, "[ ")?;
                for y in x {
                    write!(f, "{:?}; ", y)?;
                }
                write!(f, "]")
            },
            Value::Dict(ref x) => {
                write!(f, "{{ ")?;
                for (k, v) in x {
                    write!(f, "{}: {:?}; ", k, v)?;
                }
                write!(f, "}}")
            },
            Value::Tagged(ref t, ref x) => write!(f, "(#{} {:?})", t, x),
            Value::Void => write!(f, "()"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Value::Text(ref x) => write!(f, "{}", x),
            Value::Number(ref x) => write!(f, "{}", x),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Array(ref x) => {
                write!(f, "[")?;
                for y in x {
                    write!(f, "{}", y)?;
                }
                write!(f, "]")
            },
            Value::Dict(ref x) => {
                write!(f, "{{")?;
                for (k, v) in x {
                    write!(f, "{} {} ", k, v)?;
                }
                write!(f, "}}")
            },
            Value::Tagged(ref t, ref x) => write!(f, "#{} {}", t, x),
            Value::Void => write!(f, "()"),
        }
    }
}
