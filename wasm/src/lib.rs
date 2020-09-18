#![allow(dead_code, unused_variables)]

extern crate im;
extern crate js_sys;
extern crate log;
extern crate shared;
extern crate wasm_bindgen;
extern crate wasm_logger;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;
#[macro_use]
extern crate lazy_static;
use log::info;
use std::collections::HashMap;
mod unwrap;

#[derive(Default)]
struct Envs {
    count: usize,
    map: HashMap<usize, shared::types::RuntimeEnv>,
}

impl Envs {
    fn add(&mut self, env: shared::types::RuntimeEnv) -> usize {
        self.count += 1;
        self.map.insert(self.count, env);
        self.count
    }
}

lazy_static! {
    static ref ENV: Mutex<Envs> = Mutex::new(Default::default());
}

// Handlers looks like Vec<(String - hash, usize - fnid for calling back, bool - is it sync)>

struct FFI(HashMap<(String, usize, bool), js_sys::Function>);

use shared::state::FullRequest;
use shared::types::*;
use std::sync::Arc;
impl shared::ffi::FFI for FFI {
    fn handle_request_sync(
        &mut self,
        // this is the type of the constructor.
        // This gives us types of each of the arguments (in case one is a lambda)
        // and of the expected return value (so we can type-check that too)
        t: &ABT<Type>,
        kind: &Reference,
        number: usize,
        args: &Vec<Arc<Value>>,
    ) -> Option<Value> {
        match kind {
            Reference::DerivedId(Id(hash, _, _)) => {
                let js_args = js_sys::Array::new();
                for arg in args {
                    js_args.push(&crate::unwrap::unwrap(&arg));
                }
                self.0.get(&(hash.to_string(), number, true)).map(|f| {
                    let result = f
                        .apply(&JsValue::UNDEFINED, &js_args)
                        .expect("JS Function failed with an error");
                    if result.is_undefined() || result.is_null() {
                        shared::unit()
                    } else {
                        let (_, _, return_type) = shared::ir_runtime::extract_args(t);

                        match unwrap::wrap(&result, &return_type) {
                            Some(x) => x,
                            None => match result.into_serde() {
                                Err(_) => unreachable!("Not a value {:?}", result),
                                Ok(r) => r,
                            },
                        }
                    }
                })
            }
            _ => None,
        }
    }

    fn handles(&self, kind: &Reference) -> bool {
        match kind {
            Reference::DerivedId(Id(hash, _, _)) => {
                // TODO: need to iterate through all constructors
                self.0.contains_key(&(hash.to_string(), 0, false))
                    || self.0.contains_key(&(hash.to_string(), 0, true))
            }
            _ => false,
        }
    }

    // This is used at the top level, once we've bailed.
    fn handle_request(&mut self, request: FullRequest) {
        let FullRequest(kind, number, args, frames, final_index, t) = request;
        match &kind {
            Reference::DerivedId(Id(hash, _, _)) => {
                let js_args = js_sys::Array::new();
                for arg in args {
                    js_args.push(&crate::unwrap::unwrap(&arg));
                }
                js_args.push(
                    &JsValue::from_serde(&(kind.clone(), number, frames, final_index)).unwrap(),
                );
                match self.0.get(&(hash.to_string(), number, false)) {
                    Some(f) => {
                        f.apply(&JsValue::UNDEFINED, &js_args).unwrap();
                    }
                    None => unreachable!("No handler provided for {:?} # {}", hash, number),
                }
            }
            _ => unreachable!(),
        }
    }
}

impl From<Vec<JsValue>> for FFI {
    // (ability hash: string, ability index: usize, is_sync: false, handler_function: (...args) => 'a)
    // (ability hash: string, ability index: usize, is_sync: true, handler_function: (...args, kont) => 'a)
    fn from(raw_handlers: Vec<JsValue>) -> Self {
        let mut handlers = HashMap::new();
        for decl in raw_handlers {
            let decl: js_sys::Array = decl.into();
            let ability_hash = decl.get(0).as_string().unwrap();
            let ability_index = decl.get(1).as_f64().unwrap() as usize;
            let is_sync = decl.get(2).as_bool().unwrap();
            let handler_function: js_sys::Function = decl.get(3).into();
            handlers.insert((ability_hash, ability_index, is_sync), handler_function);
        }
        FFI(handlers)
    }
}

#[wasm_bindgen]
pub fn lambda(
    env_id: usize,
    partial: JsValue,
    arg: JsValue,
    raw_handlers: Vec<JsValue>,
) -> Result<JsValue, JsValue> {
    let mut ffi = FFI::from(raw_handlers);

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let value: Value = partial.into_serde().expect("Not a Value");
    let (fnid, bindings, t) = match value {
        Value::PartialFnBodyWithType(fnid, bindings, t) => (fnid, bindings, t),
        _ => unreachable!(
            "Lambda called with something other than PartialFnBodyWithType {:?}",
            value
        ),
    };

    info!("LAMBDA: type {:?}", t);

    let (arg_type, effects, res_type) = match t {
        ABT::Tm(Type::Arrow(arg, res)) => match &*res {
            ABT::Tm(Type::Effect(effects, inner)) => (
                arg,
                match &**effects {
                    ABT::Tm(Type::Effects(inner)) => inner.clone(),
                    _ => unreachable!("Invalid effects first argument: {:?}", effects),
                },
                res,
            ),
            _ => (arg, vec![], res),
        },
        _ => unreachable!("Unexpected fn type: {:?}", t),
    };
    info!("Effects: {:?}", effects);

    use std::iter::FromIterator;
    let effects_set = std::collections::HashSet::from_iter(effects.into_iter());

    let mut state = shared::state::State::lambda(
        &env,
        fnid,
        bindings,
        // value,
        shared::convert::convert_arg(WrappedValue(arg), &arg_type, vec![]).unwrap(),
        &*arg_type,
        shared::state::build_effects_map(effects_set),
    )
    .expect("Invalid Resume arg type");
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut ffi, &mut trace).unwrap();
    Ok(val
        .map(|m| crate::unwrap::unwrap(&m))
        .unwrap_or(JsValue::UNDEFINED))
}

#[wasm_bindgen]
pub fn resume(
    env_id: usize,
    kont: JsValue,
    arg: JsValue,
    raw_handlers: Vec<JsValue>,
) -> Result<JsValue, JsValue> {
    let mut ffi = FFI::from(raw_handlers);

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let (kind, constructor_no, frames, kidx): (Reference, usize, Vec<shared::frame::Frame>, usize) =
        kont.into_serde().unwrap();

    let t = env.get_ability_type(&kind, constructor_no);

    let mut state = shared::state::State::full_resume(
        &env,
        kind,
        constructor_no,
        frames,
        kidx,
        Arc::new(shared::convert::convert_arg(WrappedValue(arg), &t, vec![]).unwrap()),
    )
    .expect("Invalid Resume arg type");
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut ffi, &mut trace).unwrap();
    Ok(val
        .map(|m| unwrap::unwrap(&m))
        .unwrap_or(JsValue::UNDEFINED))
}

#[wasm_bindgen]
pub fn run_sync(
    env_id: usize,
    term: &str,
    args: Vec<JsValue>,
    raw_handlers: Vec<JsValue>,
) -> Result<JsValue, JsValue> {
    // TODO bail if any handlers aer async?
    let mut ffi = FFI::from(raw_handlers);

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let hash = shared::types::Hash::from_string(term);
    let t = &env.terms.get(&hash).unwrap().1;
    // TODO effects!
    let (targs, effects, _tres) = shared::ir_runtime::extract_args(t);
    for effect in effects.iter() {
        use shared::ffi::FFI;
        if !effect.is_var() && !ffi.handles(&effect.as_tm().unwrap().as_reference().unwrap()) {
            return Err(JsValue::from(format!(
                "Doesn't handle all effects: {:?}",
                effect
            )));
        }
    }
    let args =
        shared::convert::convert_args(args.into_iter().map(|x| WrappedValue(x)).collect(), &targs)?;

    let eval_hash = env.add_eval(term, args)?;

    let mut state = shared::state::State::new_value(
        &env,
        eval_hash,
        false,
        shared::state::build_effects_map(effects),
    );
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut ffi, &mut trace).unwrap();
    Ok(unwrap::unwrap(
        &val.expect("This was expected to be synchronous"),
    ))
}

#[wasm_bindgen]
pub fn enable_logging() {
    // CONSOLE.LOG right here to turn it on
    wasm_logger::init(wasm_logger::Config::default());
}

#[wasm_bindgen]
pub fn enable_logging_with_prefix(prefix: &str) {
    // CONSOLE.LOG right here to turn it on
    wasm_logger::init(wasm_logger::Config::default().module_prefix(prefix));
}

#[wasm_bindgen]
pub fn info(env_id: usize, term: &str) -> Result<JsValue, JsValue> {
    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let (_, typ) = env.terms.get(&Hash::from_string(term)).unwrap();
    Ok(JsValue::from_serde(&typ.args_and_effects()).unwrap())
}

#[wasm_bindgen]
pub fn run(
    env_id: usize,
    term: &str,
    args: Vec<JsValue>,
    raw_handlers: Vec<JsValue>,
) -> Result<JsValue, JsValue> {
    let mut ffi = FFI::from(raw_handlers);

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let hash = shared::types::Hash::from_string(term);
    let t = &env.terms.get(&hash).unwrap().1;
    // TODO validate that all effects are handled!
    let (targs, effects, _tres) = shared::ir_runtime::extract_args(t);
    for effect in effects.iter() {
        use shared::ffi::FFI;
        if !effect.is_var()
            && !ffi.handles(
                &effect
                    .as_tm()
                    .expect("Not a TM")
                    .as_reference()
                    .expect("Not a reference"),
            )
        {
            return Err(JsValue::from(format!(
                "Doesn't handle all effects: {:?}",
                effect
            )));
        }
    }
    let args =
        shared::convert::convert_args(args.into_iter().map(|x| WrappedValue(x)).collect(), &targs)?;

    let eval_hash = env.add_eval(term, args)?;

    let mut state = shared::state::State::new_value(
        &env,
        eval_hash,
        false,
        shared::state::build_effects_map(effects),
    );
    let mut trace = shared::chrome_trace::Traces::new();
    let _ignored = state.run_to_end(&mut ffi, &mut trace);
    Ok(JsValue::UNDEFINED)
}

#[wasm_bindgen]
pub fn load(data: &str) -> usize {
    console_error_panic_hook::set_once();
    let env = shared::unpack(data);
    ENV.lock().unwrap().add(env)
}

#[derive(Debug)]
struct WrappedValue(JsValue);

impl shared::convert::ConvertibleArg<WrappedValue> for WrappedValue {
    fn as_f64(&self) -> Option<f64> {
        self.0.as_f64()
    }
    fn as_string(&self) -> Option<String> {
        self.0.as_string()
    }
    fn as_list(&self) -> Option<Vec<Self>> {
        None
    }
    fn is_empty(&self) -> bool {
        self.0.is_null() || self.0.is_undefined()
    }
}
