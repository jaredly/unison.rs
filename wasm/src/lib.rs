extern crate im;
extern crate shared;
extern crate wasm_bindgen;
extern crate wasm_logger;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;
#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref ENV: Mutex<Option<shared::types::RuntimeEnv>> = Mutex::new(None);
}

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn load(data: &str) {
    console_error_panic_hook::set_once();
    let env = shared::unpack(data);
    *ENV.lock().unwrap() = Some(env);
}

#[wasm_bindgen]
pub fn eval_fn(hash_raw: &str, values: Vec<JsValue>) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.as_mut().unwrap();

    let hash = Hash::from_string(hash_raw);
    let t = &env.terms.get(&hash).unwrap().1;
    // TODO effects!
    let (targs, _effects, _tres) = shared::ir_runtime::extract_args(t);
    let args = convert_args(values, &targs)?;

    let eval_hash = env.add_eval(hash_raw, args)?;

    let mut state = shared::ir_runtime::State::new_value(&l.as_ref().unwrap(), eval_hash);
    let mut trace = shared::trace::Traces::new();
    let val = state.run_to_end(&mut trace);
    Ok(JsValue::from_serde(&val).unwrap())
}

#[wasm_bindgen]
pub fn evalit(hash: &str) -> JsValue {
    console_error_panic_hook::set_once();

    let mut trace = shared::trace::Traces::new();
    let l = ENV.lock().unwrap();
    let val = shared::ir_runtime::eval(&l.as_ref().unwrap(), &hash, &mut trace);
    JsValue::from_serde(&val).unwrap()
}

use shared::types::*;

fn convert_arg<'a>(
    arg: JsValue,
    typ: &'a ABT<Type>,
    mut args: Vec<&'a ABT<Type>>,
) -> Result<Value, String> {
    use Type::*;
    use ABT::*;
    match typ {
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
                _ => Err(format!("Unsupported builtin {}", name)),
            },
            Ref(Reference::DerivedId(Id(hash, _, _))) => {
                let hash_raw = hash.to_string();
                if hash_raw == shared::ir_runtime::OPTION_HASH {
                    match args.as_slice() {
                        [targ] => {
                            if arg.is_null() || arg.is_undefined() {
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

fn convert_args(args: Vec<JsValue>, typs: &Vec<ABT<Type>>) -> Result<Vec<Value>, String> {
    if args.len() > typs.len() {
        return Err("Too many arguments provided".to_owned());
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
