extern crate shared;
extern crate wasm_logger;
// extern crate base64;
extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;
// use serde_json::{Result, Value};

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

#[wasm_bindgen]
pub fn evalit(data: &str) -> JsValue {
    console_error_panic_hook::set_once();
    let (env, hash) = shared::unpack(data);

    let mut trace = shared::trace::Traces::new();
    let val = shared::ir_runtime::eval(env, &hash, &mut trace);
    JsValue::from_serde(&val).unwrap()
}
