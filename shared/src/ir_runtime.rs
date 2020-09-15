use super::types::*;
use super::types::{RuntimeEnv, IR};
use log::info;
use std::sync::Arc;

use super::chrome_trace::Traces;
use super::frame::Source;

pub fn show_env(env: &RuntimeEnv) {
    info!("[- ENV -]");
    for (k, (v, _)) in env.terms.iter() {
        info!("] Value {:?}", k);
        for (n, i) in v.iter().enumerate() {
            info!("({}) {:?}", n, i);
        }
        info!("\n");
    }
    for (i, v) in env.anon_fns.iter().enumerate() {
        info!("] Fn({}) : {:?}", i, v.0);
        for (n, i) in v.1.iter().enumerate() {
            info!("({}) {:?}", n, i)
        }
        info!("\n");
    }
}

pub fn extract_args(
    typ: &ABT<Type>,
) -> (
    Vec<ABT<Type>>,
    std::collections::HashSet<(Reference, Option<Reference>)>,
    ABT<Type>,
) {
    use Type::*;
    match typ {
        ABT::Abs(_, _, inner) => extract_args(inner),
        ABT::Tm(typ) => match typ {
            Forall(inner) => extract_args(inner),
            Effect(effects, inner) => {
                let (a, mut b, c) = extract_args(inner);
                match &**effects {
                    ABT::Tm(t) => match t {
                        Effects(effects) => {
                            for effect in effects {
                                // println!("Ok {:?}", effect);
                                match effect {
                                    ABT::Var(_, _) => (),
                                    ABT::Tm(Type::Ref(reference)) => {
                                        b.insert((reference.clone(), None));
                                    }
                                    ABT::Tm(Type::App(obj, arg)) => {
                                        let obj = match &**obj {
                                            ABT::Tm(Type::Ref(reference)) => reference,
                                            _ => unreachable!(
                                                "Effect App(x, _) must be a Ref - {:?} => {:?} => {:?}",
                                                obj, effect, typ
                                            ),
                                        };
                                        let arg = match &**arg {
                                            ABT::Tm(Type::Ref(reference)) => reference,
                                            _ => unreachable!(
                                                "Effect App(_, x) must be a Ref - {:?} => {:?} => {:?}",
                                                arg, effect, typ
                                            ),
                                        };
                                        b.insert((obj.clone(), Some(arg.clone())));
                                    }
                                    // Hmmm. How do we deal with "this reference is parameterized"
                                    _ => unreachable!("Effect not a reeference {:?}", effect),
                                }
                            }
                            // TODO go through and find the DerivedId refs, and just add those
                            // also dedup.
                            // effects.clone()
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                // b.extend(effects); // TODO do I care about ordering here?
                // for e in effects {
                //     b.push(e)
                // }
                (a, b, c)
            }
            Arrow(one, two) => {
                let (mut a, b, c) = extract_args(two);
                // println!("ARROW {:?} => {:?}", one, two);
                // let one = match &**one {
                //     ABT::Tm(t) => t.clone(),
                //     _ => unreachable!("Not a tm {:?}", one),
                // };
                a.insert(0, (**one).clone());
                (a, b, c)
            }
            Ann(t, _) => extract_args(t),
            t => (vec![], Default::default(), ABT::Tm(t.clone())),
        },
        _ => unreachable!("Um not a Tm {:?}", typ),
    }
}

pub fn eval<T: crate::ffi::FFI>(
    env: &RuntimeEnv,
    ffi: &mut T,
    hash: &str,
    trace: &mut Traces,
    do_trace: bool,
) -> Result<Option<Arc<Value>>, crate::state::InvalidFFI> {
    let mut state = crate::state::State::new_value(&env, Hash::from_string(hash), do_trace);
    state.run_to_end(ffi, trace)
}

// stack.back_to_handler -> `Handle(nidx, frames, etc.) | `TopLevel --- if we've bottomed out,
// how do we deal

pub type RunResult<T> = std::result::Result<T, crate::state::Error>;

impl RuntimeEnv {
    pub fn add_eval(&mut self, hash: &str, args: Vec<Value>) -> Result<Hash, String> {
        let typ = self.terms.get(&Hash::from_string(hash)).unwrap().1.clone();
        let mut cmds = vec![IR::Value(Value::Ref(Reference::from_hash(hash)))];
        let (_arg_typs, _effects, typ) = extract_args(&typ);
        for (_, arg) in args.into_iter().enumerate() {
            // if typ_check(&arg, arg_typs[i]) {
            cmds.push(IR::Value(arg));
            cmds.push(IR::Call);
            // } else {
            //     return Err("NOPE".to_owned());
            // };
        }
        let hash = Hash::from_string("<eval>");
        self.terms.insert(hash.clone(), (cmds, typ));
        Ok(hash)
    }

    pub fn get_ability_type(&self, kind: &Reference, number: usize) -> ABT<Type> {
        let decl = match kind {
            Reference::DerivedId(Id(hash, _, _)) => {
                self.types.get(hash).expect("Ability type not found")
            }
            _ => unreachable!("No builtin abilities"),
        };
        let data = match decl {
            TypeDecl::Effect(data) => data,
            _ => unreachable!("Not an effect type"),
        };
        let (_, constructor_type) = &data.constructors[number];
        // println!("Extracting args: {:?}", constructor_type);
        constructor_type.clone()
        // let (_arg_types, _effects, return_type) = extract_args(constructor_type);
        // return_type
    }

    // pub fn validate_ability_type(&self, kind: &Reference, number: usize, value: &Value) -> bool {
    //     let constructor_type = self.get_ability_type(kind, number);
    //     let (_arg_types, _effects, return_type) = extract_args(constructor_type);
    //     // println!("Validating Ability Type: {:?} <=> {:?}", return_type, value);
    //     crate::check::validate(Default::default(), &return_type, value).is_ok()
    // }

    pub fn cmds(&self, source: &Source) -> &Vec<IR> {
        match source {
            Source::Value(hash) => &self.terms.get(hash).unwrap().0,
            Source::Fn(fnid, _) => &self.anon_fns[*fnid].1,
        }
    }
}
