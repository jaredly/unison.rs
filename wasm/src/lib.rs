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
pub fn greet(name: &str) {
    println!("Yo folks");
    log(&format!("Hello, {}!", name));
}

pub fn load(data: &str) {
    console_error_panic_hook::set_once();
    let env = shared::unpack(data);
    *ENV.lock().unwrap() = Some(env);
}

#[wasm_bindgen]
pub fn eval_fn(hash: &str, values: Vec<JsValue>) -> JsValue {
    console_error_panic_hook::set_once();

    let args: Vec<shared::types::Value> = values
        .into_iter()
        .map(|x| x.into_serde().unwrap())
        .collect();

    let eval_hash = ENV.lock().unwrap().unwrap().add_eval(&hash, args);

    let mut state = shared::ir_runtime::State::new_value(&ENV.lock().unwrap().unwrap(), eval_hash);
    let mut trace = shared::trace::Traces::new();
    let val = state.run_to_end(&mut trace);
    JsValue::from_serde(&val).unwrap()
}

#[wasm_bindgen]
pub fn evalit(hash: &str) -> JsValue {
    console_error_panic_hook::set_once();

    let mut trace = shared::trace::Traces::new();
    let val = shared::ir_runtime::eval(&ENV.lock().unwrap().unwrap(), &hash, &mut trace);
    JsValue::from_serde(&val).unwrap()
}
