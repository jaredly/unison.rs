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
pub fn eval_fn(hash_raw: &str, values: Vec<JsValue>) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();

    let mut l = ENV.lock().unwrap();
    let env: &mut shared::types::RuntimeEnv = l.as_mut().unwrap();

    let hash = shared::types::Hash::from_string(hash_raw);
    let t = &env.terms.get(&hash).unwrap().1;
    // TODO effects!
    let (targs, _effects, _tres) = shared::ir_runtime::extract_args(t);
    let args = shared::ir_runtime::convert_args(
        values.into_iter().map(|x| WrappedValue(x)).collect(),
        &targs,
    )?;

    let eval_hash = env.add_eval(hash_raw, args)?;

    let mut state = shared::ir_runtime::State::new_value(&l.as_ref().unwrap(), eval_hash, false);
    let mut trace = shared::chrome_trace::Traces::new();
    let val = state.run_to_end(&mut trace);
    Ok(JsValue::from_serde(&val).unwrap())
}

#[wasm_bindgen]
pub fn evalit(hash: &str) -> JsValue {
    console_error_panic_hook::set_once();

    let mut trace = shared::chrome_trace::Traces::new();
    let l = ENV.lock().unwrap();
    let val = shared::ir_runtime::eval(&l.as_ref().unwrap(), &hash, &mut trace, false);
    JsValue::from_serde(&val).unwrap()
}
