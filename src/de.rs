use serde::de::{self, Deserialize, Visitor, Unexpected, Error};
use serde::de::value::{SeqDeserializer, MapDeserializer};
#[doc(inline)]
use serde::de::value::Error as VError;
use num::ToPrimitive;
use value::*;
use value::Value::*;

pub fn from_value<'a, T>(input: Value) -> Result<T, VError>
    where T: Deserialize<'a>
{
    T::deserialize(Deserializer::new(input))
}

impl<'de> de::IntoDeserializer<'de, VError> for Value {
    type Deserializer = Deserializer;

    fn into_deserializer(self) -> Deserializer {
        Deserializer::new(self)
    }
}

pub struct Deserializer {
    input: Value,
}

impl Deserializer {
    pub fn new(input: Value) -> Self {
        Deserializer { input: input }
    }

    fn unexpected<'de>(val: Value) -> Unexpected<'de> {
        // TODO: show the actual values (figure out the lifetime thing)
        match val {
            Text(_) => Unexpected::Other(&"text"),
            Number(_) => Unexpected::Other(&"number"),
            Boolean(x) => Unexpected::Bool(x),
            Array(_) => Unexpected::Seq,
            Dict(_) => Unexpected::Map,
            Tagged(_, _) => Unexpected::Other(&"tag"),
            Void => Unexpected::Unit,
        }
    }
}

impl<'de> de::Deserializer<'de> for Deserializer {
    type Error = VError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Text(x) => visitor.visit_str(&x),
            Boolean(x) => visitor.visit_bool(x),
            Array(x) => visitor.visit_seq(SeqDeserializer::new(x.iter().map(|v| (*v).clone()))),
            Dict(x) => visitor.visit_map(MapDeserializer::new(x.iter().map(|(k, v)| ((*k).clone(), (*v).clone())))),
            //Tagged(_, x) => visitor.visit_any(x),
            Void => visitor.visit_unit(),
            _ => unimplemented!(),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Void => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Text(x) => visitor.visit_string(x),
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a string")),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_i8(
                    (*x).to_integer().to_i8().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into i8",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_i16(
                    (*x).to_integer().to_i16().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into i16",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_i32(
                    (*x).to_integer().to_i32().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into i32",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_i64(
                    (*x).to_integer().to_i64().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into i64",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_u8(
                    (*x).to_integer().to_u8().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into u8",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_u16(
                    (*x).to_integer().to_u16().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into u16",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_u32(
                    (*x).to_integer().to_u32().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into u32",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_u64(
                    (*x).to_integer().to_u64().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that fits into u64",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_f32(
                    (*x).numer().to_f32().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that is valid as f32",
                    ))? /
                    (*x).denom().to_f32().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that is valid as f32",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, VError>
        where V: Visitor<'de>
    {
        match self.input {
            Number(ref x) => {
                visitor.visit_f64(
                    (*x).numer().to_f64().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that is valid as f64",
                    ))? /
                    (*x).denom().to_f64().ok_or(VError::invalid_value(
                        Deserializer::unexpected(Number((*x).clone())),
                        &"a number that is valid as f64",
                    ))?,
                )
            },
            x => Err(VError::invalid_type(Deserializer::unexpected(x), &"a number")),
        }
    }

    forward_to_deserialize_any! {
        bool str char bytes
        byte_buf unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

#[cfg(test)]
mod tests {
    //use value::*;
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_void() {
        assert_eq!((), from_value(Value::Void).unwrap());
    }

    #[test]
    fn test_numbers() {
        assert_eq!(-128, from_value::<i8>(int(b"-128", 10)).unwrap());
        assert_eq!(
            Err(VError::custom("invalid value: number, expected a number that fits into i8")),
            from_value::<i8>(int(b"128", 10))
        );
        assert_eq!(2.5, from_value::<f32>(rat(b"5", b"2", 10)).unwrap());
    }

    #[test]
    fn test_text() {
        assert_eq!("hello".to_owned(), from_value::<String>(Value::Text("hello".to_owned())).unwrap());
    }

    #[test]
    fn test_array() {
        assert_eq!(
            vec!["hello".to_owned(), "world".to_owned()],
            from_value::<Vec<String>>(arr(vec![Value::Text("hello".to_owned()), Value::Text("world".to_owned())])).unwrap()
        );
    }

    #[test]
    fn test_dict() {
        let mut btm = BTreeMap::new();
        btm.insert("hello".to_owned(), "world".to_owned());
        assert_eq!(
            btm,
            from_value::<BTreeMap<String, String>>(Value::Dict(map!{"hello".to_owned() => Value::Text("world".to_owned())})).unwrap()
        );
    }

    #[test]
    fn test_struct() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct Y {
            count: Option<u32>,
            x: Option<u32>,
        }
        #[derive(Deserialize, Debug, PartialEq)]
        struct X {
            name: String,
            stuff: Box<Y>,
        }
        assert_eq!(
            X {
                name: "test".to_owned(),
                stuff: Box::new(Y { count: Some(123), x: None }),
            },
            from_value::<X>(Value::Dict(
                    map!{"name".to_owned() => Value::Text("test".to_owned()), "stuff".to_owned() => Value::Dict(map!{ "count".to_owned() => int(b"123", 10) })},
                )).unwrap()
        );
        #[derive(Deserialize, Debug, PartialEq)]
        struct U;
        assert_eq!(U, from_value::<U>(Value::Void).unwrap());
    }

    #[test]
    fn test_enum() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum X {
            One(u32),
            Two,
            Three { f: String },
        }
        assert_eq!(X::One(0), from_value::<X>(Value::Tagged("One".to_string(), Box::new(int(b"0", 10)))).unwrap());
        assert_eq!(X::Two, from_value::<X>(Value::Tagged("Two".to_string(), Box::new(Value::Void))).unwrap());
        assert_eq!(X::Three { f: "Hello".to_string() },
            from_value::<X>(Value::Tagged(
                "Three".to_string(),
                Box::new(Value::Dict(map!{ "f".to_owned() => Value::Text("hello".to_owned()) })),
            )).unwrap());
    }

}
