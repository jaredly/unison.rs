// #ed72l2mrh0
use shared::state::{FullRequest, InvalidFFI, State};
use shared::types::*;

fn unit() -> Value {
    Value::Constructor(
    Reference::DerivedId(Id::from_string(
        "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8"
    )), 0)
}

pub struct RustFFI(pub crate::printer::FlatNames, pub Vec<FullRequest>);
impl RustFFI {
    pub fn has_next_request(&self) -> bool {
        self.1.len() > 0
    }

    pub fn process_next_request(
        &mut self,
        env: &RuntimeEnv,
        trace: &mut shared::chrome_trace::Traces,
    ) -> Result<(), InvalidFFI> {
        let FullRequest(kind, number, args, frames, final_index, _t) = self.1.remove(0);
        match &kind {
            Reference::DerivedId(id) => match (&id.to_string()[0..10], number) {
                ("onasci86q4", 0) => {
                    State::full_resume(
                        env,
                        kind.clone(),
                        number,
                        frames.clone(),
                        final_index,
                        args[0].clone(),
                    )?
                    .run_to_end(self, trace)?
                    .map(|v| {
                        println!(
                            "first run -> {}",
                            crate::printer::value_to_pretty(&v, &self.0, 100)
                        )
                    });
                    State::full_resume(env, kind, number, frames, final_index, args[1].clone())?
                        .run_to_end(self, trace)?
                        .map(|v| {
                            println!(
                                "second run -> {}",
                                crate::printer::value_to_pretty(&v, &self.0, 100)
                            )
                        });
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        Ok(())
    }
}

impl shared::ffi::FFI for RustFFI {
    fn handle_request_sync(
        &mut self,
        _t: &ABT<Type>,
        kind: &Reference,
        number: usize,
        args: &Vec<std::sync::Arc<Value>>,
    ) -> Option<Value> {
        // println!("Asking for a request {:?} : {} : {:?}", kind, number, args);
        match kind {
            Reference::DerivedId(id) => {
                match &id.to_string()[0..10] {
                    "ed72l2mrh0" => {
                        // currentTimeStampMs
                        if number == 0 {
                            Some(Value::Nat(
                                std::time::SystemTime::UNIX_EPOCH
                                    .elapsed()
                                    .unwrap()
                                    .as_millis() as u64,
                            ))
                        } else {
                            unreachable!();
                        }
                    }
                    "s81fshin91" => {
                        println!(
                            "[LOG] {}",
                            args.iter()
                                .map(|arg| crate::printer::value_to_pretty(arg, &self.0, 100))
                                .collect::<Vec<String>>()
                                .join(", ")
                        );
                        return Some(unit());
                        // number 0, 1, 2 determines log, error or warn
                        // if number === "log"
                    }
                    "mvd13op0i1" => {
                        match *args[0] {
                            Value::Nat(millis) => {
                                std::thread::sleep(std::time::Duration::from_millis(millis))
                            }
                            _ => unreachable!("Expected a Nat"),
                        }
                        Some(unit())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn handles(&self, kind: &Reference) -> bool {
        match kind {
            Reference::DerivedId(id) => match &id.to_string()[0..10] {
                "onasci86q4" => true,
                "ed72l2mrh0" => true,
                "s81fshin91" => true,
                "mvd13op0i1" => true,
                _ => false,
            },
            _ => false,
        }
    }

    // This is used at the top level, once we've bailed.
    fn handle_request(&mut self, request: FullRequest) {
        self.1.push(request);
    }
}
