use crate::types::*;

const UNIT_HASH: &'static str = "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8";
// const TUPLE_HASH: &'static str = "onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0";
const FFI_HASH: &'static str = "ne22tbsth76tbte4ancb6p62khuv2c5jdegm4jk44o4n0nlou12inmjjldbjlo5hp66nqo8j55qglirsc1ecp9ea2ofcr0it9od3cl0";

pub fn validate(
    bindings: im::HashMap<String, ABT<Type>>,
    typ: &ABT<Type>,
    val: &Value,
) -> Result<(), Vec<String>> {
    use ABT::*;
    match typ {
        Var(_, _) => unimplemented!("Var"),
        Cycle(content) => validate(bindings, &*content, val),
        Abs(_sym, _, _content) => unimplemented!("Nope"),
        Tm(typ) => match typ {
            Type::Ref(Reference::Builtin(name)) => match (name.as_str(), val) {
                ("Int", Value::Int(_)) => Ok(()),
                ("Nat", Value::Nat(_)) => Ok(()),
                ("Float", Value::Float(_)) => Ok(()),
                ("Boolean", Value::Boolean(_)) => Ok(()),
                ("Text", Value::Text(_)) => Ok(()),
                ("Bytes", Value::Bytes(_)) => Ok(()),
                ("Char", Value::Char(_)) => Ok(()),
                _ => Err(vec![format!("Expected {}, found {:?}", name, val)]),
            },
            Type::Ref(Reference::DerivedId(Id(hash, _, _))) => {
                if hash.0 == UNIT_HASH {
                    match val {
                        Value::Constructor(Reference::DerivedId(Id(hi, _, _)), 0)
                            if hi.0 == UNIT_HASH =>
                        {
                            Ok(())
                        }
                        _ => Err(vec!["Expected a unit".to_owned()]),
                    }
                } else {
                    unimplemented!("Can't do custom types yet: {:?}", hash)
                }
            }
            Type::Ann(inner, _) => validate(bindings, inner, val),
            Type::Effect(_, inner) => validate(bindings, inner, val),
            Type::Effects(_) => unimplemented!("Effects"),
            Type::Forall(_) => unimplemented!("Forall"),
            Type::IntroOuter(_) => unimplemented!("IntroOuter"),
            Type::Arrow(_arg, _body) => match val {
                _ => unimplemented!("Sorry, arrow not yet supported"), // ugh, I don't have enough type information right here
            },
            Type::App(inner, v) => match &**inner {
                Tm(Type::Ref(Reference::DerivedId(Id(hash, _, _)))) if hash.0 == FFI_HASH => {
                    // this is my custom FFI type -- this is allowed without introspection...
                    Ok(())
                }
                Abs(sym, _, inner) => validate(
                    {
                        let mut bindings = bindings.clone();
                        bindings.insert(sym.text.clone(), (**v).clone());
                        bindings
                    },
                    inner,
                    val,
                ),
                nested => unreachable!("Arrow must have abs inside - {:?} <-> {:?}", nested, v),
            },
        },
    }
}
