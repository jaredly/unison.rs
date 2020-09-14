pub mod check;
pub mod chrome_trace;
pub mod frame;
pub mod ir_exec;
pub mod ir_runtime;
pub mod pattern;
pub mod stack;
pub mod trace;
pub mod types;

pub fn unit() -> types::Value {
    types::Value::Constructor(
    types::Reference::DerivedId(types::Id(types::Hash::from_string(
        "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8"
    ), 0, 1)), 0)
}

pub fn pack(env: &types::RuntimeEnv) -> String {
    let encoded: Vec<u8> = bincode::serialize(&env).unwrap();
    base64::encode(&encoded)
}

pub fn unpack(data: &str) -> types::RuntimeEnv {
    let raw = base64::decode(data).expect("Cannot base64 unpack");
    bincode::deserialize(&raw).expect("Unable to deserialize")
}
