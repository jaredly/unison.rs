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

// OK FOLKS heres what we're doing
/*

So the concrete type is like:
    App(Ref(#thing), Ref(Nat))

And the inner type is like:
    Forall(|a| Arrow(a -> Effect(_effects, App(Ref(#b)))))

// Ok, so anywhere we so `a`, substitute `Nat`
// Ok, so I think my strategy is:
// Make a new concretized type, and then pass it to `extract_args`.
// Sounds great.
Forall(|a/0 #0|
    (Arrow(
        App(
            Ref(#ne22tbsth7),
            App(Ref(#b8hn9sq0fe), 〰️a (#0))
        ),
        Effect(
            Effects([App(Ref(#ie7ejjokeb), 〰️a (#0))]),
            〰️a (#0)
        )
    ))
)


*/

pub fn extract_args(
    typ: &ABT<Type>,
) -> (
    Vec<ABT<Type>>,
    std::collections::HashSet<ABT<Type>>,
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
                                b.insert(effect.clone());
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                (a, b, c)
            }
            Arrow(one, two) => {
                let (mut a, b, c) = extract_args(two);
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
    effects: std::collections::HashMap<String, ABT<Type>>,
) -> Result<Option<Arc<Value>>, crate::state::InvalidFFI> {
    let mut state = crate::state::State::new_value(&env, Id::from_string(hash), do_trace, effects);
    state.run_to_end(ffi, trace)
}

// stack.back_to_handler -> `Handle(nidx, frames, etc.) | `TopLevel --- if we've bottomed out,
// how do we deal

pub type RunResult<T> = std::result::Result<T, crate::state::Error>;

impl RuntimeEnv {
    pub fn add_eval(&mut self, hash: &str, args: Vec<Value>) -> Result<Id, String> {
        let typ = self.terms.get(&Id::from_string(hash)).unwrap().1.clone();
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
        let hash = Id::from_string("<eval>");
        self.terms.insert(hash.clone(), (cmds, typ));
        Ok(hash)
    }

    pub fn get_ability_type(&self, kind: &Reference, number: usize) -> ABT<Type> {
        let decl = match kind {
            Reference::DerivedId(id) => self.types.get(&id).expect("Ability type not found"),
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
