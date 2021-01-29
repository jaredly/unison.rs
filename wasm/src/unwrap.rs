use shared::types::*;
use wasm_bindgen::JsValue;

const UNIT_HASH: &'static str = "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8";
const TUPLE_HASH: &'static str = "onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0";

pub fn unwrap_tuple(value: &Value) -> Vec<&Value> {
    use Value::*;
    match value {
        PartialConstructor(Reference::DerivedId(id), 0, args) => {
            if id.hash.0 == TUPLE_HASH {
                let mut res = unwrap_tuple(&args[1]);
                res.insert(0, &args[0]);
                return res;
            } else if id.hash.0 == UNIT_HASH {
                return vec![];
            }
        }
        Constructor(Reference::DerivedId(id), 0) => {
            if id.hash.0 == UNIT_HASH {
                return vec![];
            }
        }
        _ => (),
    };
    return vec![value];
}

pub fn wrap(value: &JsValue, typ: &ABT<Type>) -> Option<Value> {
    use Value::*;
    match typ {
        ABT::Tm(Type::Ref(Reference::Builtin(name))) => match name.as_str() {
            "Nat" => value.as_f64().map(|x| Nat(x as u64)),
            "Int" => value.as_f64().map(|x| Int(x as i64)),
            "Float" => value.as_f64().map(|x| Float(x)),
            "Text" => value.as_string().map(|x| Text(x)),
            "Char" => value.as_string().map(|x| Char(x.chars().next().unwrap())),
            _ => None,
        },
        _ => None,
    }
}

pub fn unwrap(value: &Value) -> JsValue {
    if value.is_constr(TUPLE_HASH) {
        let arr = js_sys::Array::new();
        for item in unwrap_tuple(value) {
            arr.push(&unwrap(item));
        }
        arr.into()
    } else if value.is_constr(UNIT_HASH) {
        JsValue::null()
    } else {
        use Value::*;
        match value {
            Nat(i) => JsValue::from_f64(*i as f64),
            Int(i) => JsValue::from_f64(*i as f64),
            Float(i) => JsValue::from_f64(*i as f64),
            Boolean(i) => JsValue::from(*i),
            Text(t) => JsValue::from(t),
            Char(c) => JsValue::from(c.to_string()),
            Sequence(items) => {
                let arr = js_sys::Array::new();
                for item in items {
                    arr.push(&unwrap(item));
                }
                arr.into()
            }
            // TODO: Should I try converting custom types?
            // Don't need to mess with that just yet.
            _ => JsValue::from_serde(value).unwrap(),
        }
    }
}
