#![allow(dead_code, unused_variables)]

extern crate im;
extern crate js_sys;
extern crate shared;
extern crate wasm_bindgen;
extern crate wasm_logger;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;
#[macro_use]
extern crate lazy_static;
use std::collections::HashMap;

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

// #[derive(Serialize, Deserialize)]
// struct Handlers(Vec<(String, bool, usize)>);

// Handlers looks like Vec<(String - hash, usize - fnid for calling back)>

struct FFI(HashMap<(String, usize), js_sys::Function>);

use shared::types::*;
use std::sync::Arc;
impl shared::ir_runtime::FFI for FFI {
    fn handle_request_sync(
        &mut self,
        kind: &Reference,
        number: usize,
        args: &Vec<Arc<Value>>,
    ) -> Option<Value> {
        None
    }

    fn handles(&self, kind: &Reference) -> bool {
        false
    }

    // This is used at the top level, once we've bailed.
    fn handle_request(&mut self, request: shared::ir_runtime::FullRequest) {
        // ok
    }
}

impl From<Vec<JsValue>> for FFI {
    fn from(raw_handlers: Vec<JsValue>) -> Self {
        let mut handlers = HashMap::new();
        for decl in raw_handlers {
            let decl: js_sys::Array = decl.into();
            let ability_hash = decl.get(0).as_string().unwrap();
            let ability_index = decl.get(1).as_f64().unwrap() as usize;
            let handler_function: js_sys::Function = decl.get(2).into();
            handlers.insert((ability_hash, ability_index), handler_function);
        }
        FFI(handlers)
    }
}

#[wasm_bindgen]
pub fn run_sync(
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
    // TODO effects!
    let (targs, _effects, _tres) = shared::ir_runtime::extract_args(t);
    let args = shared::ir_runtime::convert_args(
        args.into_iter().map(|x| WrappedValue(x)).collect(),
        &targs,
    )?;

    let eval_hash = env.add_eval(term, args)?;

    let mut state = shared::ir_runtime::State::new_value(&env, eval_hash, false);
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut ffi, &mut trace).unwrap();
    Ok(JsValue::from_serde(&val).unwrap())
}

// #[wasm_bindgen]
// extern "C" {
//     #[wasm_bindgen(js_namespace = console)]
//     fn log(s: &str);
// }

#[wasm_bindgen]
pub fn load(data: &str) -> usize {
    console_error_panic_hook::set_once();
    let env = shared::unpack(data);
    ENV.lock().unwrap().add(env)
}

#[derive(Debug)]
struct WrappedValue(JsValue);

impl shared::ir_runtime::ConvertibleArg<WrappedValue> for WrappedValue {
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
    // fn from_value()
}

#[wasm_bindgen]
pub fn eval_fn(env_id: usize, hash_raw: &str, values: Vec<JsValue>) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();

    let mut ffi = FFI(HashMap::new());

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.map.get_mut(&env_id).unwrap();

    let hash = shared::types::Hash::from_string(hash_raw);
    let t = &env.terms.get(&hash).unwrap().1;
    // TODO effects!
    let (targs, _effects, _tres) = shared::ir_runtime::extract_args(t);
    let args = shared::ir_runtime::convert_args(
        values.into_iter().map(|x| WrappedValue(x)).collect(),
        &targs,
    )?;

    let eval_hash = env.add_eval(hash_raw, args)?;

    let mut state =
        shared::ir_runtime::State::new_value(&l.map.get(&env_id).unwrap(), eval_hash, false);
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut ffi, &mut trace).unwrap();
    Ok(JsValue::from_serde(&val).unwrap())
}

#[wasm_bindgen]
pub fn evalit(env_id: usize, hash: &str) -> JsValue {
    console_error_panic_hook::set_once();

    let mut ffi = FFI(HashMap::new());

    let mut trace = shared::chrome_trace::Traces::new();
    let l = ENV.lock().unwrap();
    let val = shared::ir_runtime::eval(
        &l.map.get(&env_id).unwrap(),
        &mut ffi,
        &hash,
        &mut trace,
        false,
    )
    .unwrap();
    JsValue::from_serde(&val).unwrap()
}
