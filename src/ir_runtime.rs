use super::ir::{GlobalEnv, IR};
use super::types::*;
use log::info;
use std::rc::Rc;

use super::frame::Source;
use super::ir_exec::Ret;
use super::stack::Stack;
use super::trace::Trace;

static OPTION_HASH: &'static str = "5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8";

#[allow(while_true)]
pub fn eval(env: GlobalEnv, hash: &str, trace: &mut Vec<Trace>) -> Rc<Value> {
    let hash = Hash::from_string(hash);
    info!("[- ENV -]");
    for (k, v) in env.terms.iter() {
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

    let mut cmds: &Vec<IR> = env.terms.get(&hash).unwrap();

    let mut stack = Stack::new(Source::Value(hash.clone()));

    let option_ref = Reference::from_hash(OPTION_HASH);

    let mut idx = 0;

    let mut n = 0;

    let start = std::time::Instant::now();

    while idx < cmds.len() {
        if n % 100 == 0 {
            if start.elapsed().as_secs() > 20 {
                let n = Rc::new(Value::Text(format!("Ran out of time after {} ticks", n)));
                return n;
            }
        }

        let cstart = std::time::Instant::now();
        let cidx = idx;

        n += 1;

        let cmd = &cmds[idx];
        info!("----- <{}>    {:?}", idx, cmd);

        let ret = cmd.eval(&option_ref, &mut stack, &mut idx);

        let ctime = cstart.elapsed();
        if ctime.as_millis() > 1 {
            trace.push(Trace {
                cat: "Instruction".to_owned(),
                ph: "B".to_owned(),
                ts: start.elapsed() - ctime,
                name: (stack.frames[0].source.clone(), Some(cidx)),
                file: "".to_owned(),
                tid: 1,
            });

            trace.push(Trace {
                cat: "Instruction".to_owned(),
                ph: "E".to_owned(),
                ts: start.elapsed(),
                name: (stack.frames[0].source.clone(), Some(cidx)),
                file: "".to_owned(),
                tid: 1,
            });
        };

        match ret {
            Ret::Nothing => (),
            Ret::Handle(mark_idx) => {
                idx += 1;
                info!("Setting handle, mark idx {}", mark_idx);
                stack.frames[0].handler = Some(mark_idx);
                stack.clone_frame(mark_idx);
                stack.frames[0].handler = None;
            }
            Ret::Continue(kidx, mut frames, arg) => {
                info!("** CONTINUE ** ({}) {} with {:?}", kidx, frames.len(), arg,);
                let last = frames.len() - 1;
                frames[last].return_index = idx;
                stack.frames = {
                    frames.extend(stack.frames);
                    frames
                };
                for frame in &stack.frames {
                    info!(
                        "> {:?} : returning to index {} below",
                        frame.source, frame.return_index
                    );
                }
                idx = kidx;
                stack.push(arg);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                }
            }
            Ret::ReRequest(kind, number, args, final_index, mut frames) => {
                let (nidx, saved_frames) = match stack.back_again_to_handler() {
                    None => unreachable!("Unhandled ReRequest: {:?} / {}", kind, number),
                    Some((a, b)) => (a, b),
                };
                frames.extend(saved_frames);
                idx = nidx;
                info!(
                    "Handling a bubbled request : {} - {:?}",
                    idx, stack.frames[0].source
                );

                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };

                stack.push(Rc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    frames,
                )))
            }
            Ret::Request(kind, number, args) => {
                let final_index = idx;
                let (nidx, saved_frames) = match stack.back_to_handler() {
                    None => unreachable!("Unhandled Request: {:?} / {}", kind, number),
                    Some((a, b)) => (a, b),
                };
                idx = nidx;
                info!(
                    "Jumping back to idx {} to {:?}",
                    idx, stack.frames[0].source
                );

                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };

                stack.push(Rc::new(Value::RequestWithContinuation(
                    kind,
                    number,
                    args,
                    final_index,
                    saved_frames,
                )))
            }
            Ret::FnCall(fnid, bindings, arg) => {
                cmds = &env.anon_fns[fnid].1;

                stack.new_frame(idx, Source::Fn(fnid, env.anon_fns[fnid].0.clone()));
                trace.push(stack.frames[0].as_trace("B", start.elapsed()));
                // for binding in &bindings {
                //     info!("> {:?} = {:?}", binding.0, binding.2);
                // }
                info!("^ fn call with {:?}", arg);
                stack.frames[0].bindings = bindings;
                stack.frames[0].stack.push(arg);
                idx = 0;
            }
            Ret::Value(hash) => {
                // info!("Jumping to {:?}", hash);
                cmds = env.terms.get(&hash).unwrap();
                stack.new_frame(idx, Source::Value(hash));
                trace.push(stack.frames[0].as_trace("B", start.elapsed()));
                idx = 0;
            }
            Ret::HandlePure => {
                let (idx1, value) = stack.pop_frame();
                idx = idx1;
                stack.push(value);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => cmds = env.terms.get(&hash).unwrap(),
                    Source::Fn(fnid, _) => cmds = &env.anon_fns[fnid].1,
                };
            }
        }

        // Multi-pop
        while idx >= cmds.len() {
            trace.push(stack.frames[0].as_trace("E", start.elapsed()));
            if stack.frames.len() > 1 {
                // info!("<<-- jump down");
                let (idx1, value) = stack.pop_frame();
                idx = idx1;
                // stack.pop_frame();
                // stack.frames.remove(0);
                match stack.frames[0].source.clone() {
                    Source::Value(hash) => {
                        // info!("Going back to {}", hash);
                        stack.push(value);
                        cmds = env.terms.get(&hash).unwrap();
                    }
                    Source::Fn(fnid, _) => {
                        // info!("Going back to fn {}", fnid);
                        stack.push(value);
                        cmds = &env.anon_fns[fnid].1;
                    }
                }
            } else {
                info!("Got only one frame left, and idx is larger than the cmds len");
                break;
            }
        }
    }

    info!("Final stack: {:?}", stack);
    stack.pop().unwrap()
}
