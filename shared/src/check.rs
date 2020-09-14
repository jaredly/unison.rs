use crate::types::*;

pub fn validate(
    bindings: im::HashMap<String, ABT<Type>>,
    typ: &ABT<Type>,
    val: &Value,
) -> Result<(), Vec<String>> {
    use ABT::*;
    match typ {
        Var(_, _) => unimplemented!("Var"),
        Cycle(content) => validate(bindings, &*content, val),
        Abs(sym, _, content) => unimplemented!("Nope"),
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
                unimplemented!("Can't do custom types yet")
            }
            Type::Ann(inner, _) => validate(bindings, inner, val),
            Type::Effect(_, inner) => validate(bindings, inner, val),
            Type::Effects(_) => unimplemented!("Effects"),
            Type::Forall(_) => unimplemented!("Forall"),
            Type::IntroOuter(_) => unimplemented!("IntroOuter"),
            Type::Arrow(arg, body) => match val {
                _ => unimplemented!("Sorry, arrow not yet supported"), // ugh, I don't have enough type information right here
            },
            Type::App(inner, v) => match &**inner {
                Abs(sym, _, inner) => validate(
                    {
                        let mut bindings = bindings.clone();
                        bindings.insert(sym.text.clone(), (**v).clone());
                        bindings
                    },
                    inner,
                    val,
                ),
                _ => unreachable!("Arrow must have abs inside"),
            },
        },
    }
}
