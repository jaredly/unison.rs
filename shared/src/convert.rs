use super::types::*;
// use super::types::{RuntimeEnv, IR};
// use log::info;
// use std::sync::Arc;
// use super::chrome_trace::Traces;
// use super::frame::Source;

pub static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";
pub const UNIT_HASH: &'static str = "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8";
pub const TUPLE_HASH: &'static str = "onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0";

pub fn to_json_type(typ: ABT<Type>) -> serde_json::Value {
    use serde_json::Value::*;
    use std::iter::FromIterator;
    use Type::*;
    match typ {
        ABT::Tm(term) => match term {
            Ann(inner, _) => to_json_type(*inner),
            Forall(inner) => to_json_type(*inner),
            IntroOuter(inner) => to_json_type(*inner),
            App(one, two) => Object(serde_json::Map::from_iter(vec![
                ("type".into(), to_json_type(*one)),
                ("arg".into(), to_json_type(*two)),
            ])),
            Ref(Reference::Builtin(text)) => String(text.to_owned()),
            Ref(Reference::DerivedId(Id(hash, _, _))) => {
                if hash.0 == OPTION_HASH {
                    String("Option".to_owned())
                } else if hash.0 == UNIT_HASH {
                    Null
                } else if hash.0 == TUPLE_HASH {
                    String("Tuple".to_owned())
                } else {
                    String("UNKNOWN_CUSTOM".to_owned())
                }
            }
            _ => String("UNKNOWN_TYPE".to_owned()),
        },
        ABT::Cycle(inner) => to_json_type(*inner),
        ABT::Abs(_, _, inner) => to_json_type(*inner),
        ABT::Var(sym, _) if sym.text == "()" => serde_json::Value::Null,
        _ => String("UNKNOWN_ABT".to_owned()),
    }
}

pub trait ConvertibleArg<T: Sized> {
    fn as_f64(&self) -> Option<f64>;
    fn as_string(&self) -> Option<String>;
    fn as_list(&self) -> Option<Vec<T>>;
    fn is_empty(&self) -> bool;
}

pub fn convert_arg<'a, T>(
    arg: T,
    typ: &'a ABT<Type>,
    mut args: Vec<&'a ABT<Type>>,
) -> Result<Value, String>
where
    T: std::fmt::Debug + Sized + ConvertibleArg<T>,
{
    use Type::*;
    use ABT::*;
    match typ {
        Var(_, _) => Ok(Value::Int(10000)),
        Tm(inner) => match inner {
            Arrow(_, _) => Err("Functions aren't yet supported".to_owned()),
            Ann(inner, _) => convert_arg(arg, inner, args),
            App(inner, targ) => {
                args.insert(0, targ);
                convert_arg(arg, inner, args)
            }
            Effect(_, _) => Err("Effect types not yet supported".to_owned()),
            Effects(_) => Err("Effects not supported".to_owned()),
            Forall(inner) => convert_arg(arg, inner, args),
            IntroOuter(inner) => convert_arg(arg, inner, args),
            Ref(Reference::Builtin(name)) => match name.as_str() {
                "Nat" => match arg.as_f64() {
                    None => Err(format!("Expected an unsigned int, got {:?}", arg)),
                    Some(n) if n < 0.0 => {
                        Err(format!("Expected an unsigned int, got a negative {}", n))
                    }
                    Some(n) if n.fract() > 1.0e-10 => {
                        Err(format!("Expected an unsigned int, got a float {}", n))
                    }
                    Some(n) => Ok(Value::Nat(n as u64)),
                },
                "Int" => match arg.as_f64() {
                    None => Err(format!("Expected an int, got {:?}", arg)),
                    Some(n) if n.fract() > 1.0e-10 => {
                        Err(format!("Expected an int, got a float {}", n))
                    }
                    Some(n) => Ok(Value::Int(n as i64)),
                },
                "Float" => match arg.as_f64() {
                    None => Err(format!("Expected a float, got {:?}", arg)),
                    Some(n) => Ok(Value::Float(n)),
                },
                "Text" => match arg.as_string() {
                    None => Err(format!("Expected a string, got {:?}", arg)),
                    Some(n) => Ok(Value::Text(n)),
                },
                "Char" => match arg.as_string() {
                    None => Err(format!("Expected a string, got {:?}", arg)),
                    Some(n) => Ok(Value::Char(n.chars().next().unwrap())),
                },
                _ => Err(format!("Unsupported builtin {}", name)),
            },
            Ref(Reference::DerivedId(Id(hash, _, _))) => {
                let hash_raw = hash.to_string();
                if hash_raw == UNIT_HASH {
                    if arg.is_empty() {
                        Ok(crate::unit())
                    } else {
                        Err(format!("Expected null/undefined"))
                    }
                } else if hash_raw == OPTION_HASH {
                    match args.as_slice() {
                        [targ] => {
                            if arg.is_empty() {
                                Ok(Value::PartialConstructor(
                                    Reference::DerivedId(Id(hash.clone(), 0, 0)),
                                    0,
                                    im::Vector::new(),
                                ))
                            } else {
                                convert_arg(arg, targ, vec![])
                            }
                        }
                        _ => Err(format!("Option type can only have one argument")),
                    }
                } else {
                    Err(format!("Custom types not yet supported: {:?}", hash))
                }
            }
        },
        typ => Err(format!("Unexpected ABT {:?}", typ)),
    }
}

pub fn convert_args<T>(args: Vec<T>, typs: &Vec<ABT<Type>>) -> Result<Vec<Value>, String>
where
    T: std::fmt::Debug + Sized + ConvertibleArg<T>,
{
    if args.len() > typs.len() {
        return Err(format!(
            "Too many arguments provided: {} vs {}",
            args.len(),
            typs.len()
        ));
    }
    let mut res = vec![];
    for (i, arg) in args.into_iter().enumerate() {
        res.push(
            convert_arg(arg, &typs[i], vec![])
                .map_err(|v| format!("Unable to convert argument {}: {}", i, v))?,
        );
    }
    Ok(res)
}
