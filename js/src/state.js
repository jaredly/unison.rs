import clone from 'clone-deep';
import { Stack } from './stack';
import { eval_ir } from './ir_exec';
import { pretty_print } from './pretty_print';

const unit_hash =
    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8';

const unit_value = () => {
    return {
        Constructor: [
            {
                DerivedId: [
                    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8',
                    ,
                    0,
                    1,
                ],
            },
            0,
        ],
    };
};

export class State {
    constructor(env, ffi, debug = {}) {
        this.debug = debug;
        this.trace = {};
        this.ffi = ffi;
        this.idx = 0;
        this.env = env;
        this.cmds = [];
        this.stack = null;
        // if (!ffi) {
        //     throw new Error('No ffi');
        // }
    }

    loadHash(hash) {
        // find by prefix
        /* istanbul ignore next */
        if (!this.env.terms[hash]) {
            for (let k of Object.keys(this.env.terms)) {
                if (k.startsWith(hash)) {
                    hash = k;
                    break;
                }
            }
            /* istanbul ignore next */
            if (!this.env.terms[hash]) {
                throw new Error(`Hash not found ${hash}`);
            }
        }
        this.cmds = this.env.terms[hash][0];
        this.stack = new Stack({ Value: hash });
    }

    // STOPSHIP fullResume needs to be an alternative constructor
    fullResume(kont, arg) {
        // throw new Error('MAKE A CONSTRUCTOR SOMEHOW');
        const [hash, number, frames, final_idx] = kont;

        arg = arg != null ? arg : unit_value();

        this.idx = final_idx;
        this.stack = Stack.fromFrames(frames);
        this.stack.push(arg);
        // console.log('pushed', state.stack, arg);
        this.cmds = this.env.cmds(this.stack._frames[0].source);
    }

    /* istanbul ignore next */
    pretty_print(value) {
        return pretty_print(this.env.names, value);
    }

    run_to_end() {
        // console.log(this.cmds);
        const result = this.run();
        // TODO think about using exception handling?
        // would be interesting to benchmark
        if (result && result.FullRequest) {
            const [
                hash,
                number,
                args,
                frames,
                final_idx,
                _, // return_type,
            ] = result.FullRequest;
            const kont = [hash, number, frames, final_idx];
            const allArgs = args.slice();
            allArgs.push(kont);
            this.ffi[hash][number].fn(...allArgs);
            return null; // indicates that we are async
        }
        return this.stack.pop();
    }

    run() {
        while (this.idx < this.cmds.length) {
            // if (DEBUG) {
            //     console.log(
            //         `- ${this.stack.frames.length
            //             .toString()
            //             .padStart(3, '0')} : (${this.idx
            //             .toString()
            //             .padStart(3, '0')})`,
            //         this.cmds[this.idx],
            //     );
            // }
            const start = Date.now();
            const idx = this.idx;
            const cmd = this.cmds[this.idx];
            const tid = this.stack.currentFrame().trace_id;
            let eidx = null;
            /* istanbul ignore next */
            if (this.stack.trace) {
                this.stack.trace[tid].events.push({
                    IR: [idx, cmd],
                    // type: 'ir',
                    // idx,
                    // cmd,
                    // start,
                    // ret,
                    // end: Date.now(),
                });
                eidx = this.stack.trace[tid].events.length - 1;
            }
            const ret = eval_ir(cmd, this);
            // this.stack.trace[tid].events[eidx].end = Date.now();
            if (ret) {
                /* istanbul ignore next */
                if (this.stack.trace) {
                    this.stack.trace[
                        this.stack.currentFrame().trace_id
                    ].events.push({ Ret: ret });
                }
                const res = this.handle_ret(ret);
                // Ok folks, got to bubble
                if (res && res.FullRequest) {
                    // AHH HERE WE GOOOOOOO
                    // STOPSHIP here it is folks.
                    return res;
                }
            }
            this.handle_tail();
        }
    }

    handle_tail() {
        while (this.idx >= this.cmds.length) {
            if (!this.stack.isLastFrame()) {
                const [idx1, value] = this.stack.pop_frame();
                this.idx = idx1;
                this.stack.push(value);
                this.cmds = this.env.cmds(this.stack.currentFrame().source);
            } else {
                return;
            }
        }
    }

    handle_ret(ret) {
        const typ = typeof ret === 'string' ? ret : Object.keys(ret)[0];
        /* istanbul ignore next */
        if (this.rets[typ] == null) {
            throw new Error(
                `Cannot handle ret ${JSON.stringify(ret)} : ${typ}`,
            );
        }
        if (this.debug.ret) {
            console.log('RET', typ, ret[typ]);
        }
        return this.rets[typ](ret[typ]);
    }

    rets = {
        Handle: (mark_idx) => {
            this.idx += 1;
            /* istanbul ignore next */
            if (this.stack.currentFrame().handler) {
                throw new Error('Stack frame already has a handler');
            }
            this.stack.currentFrame().handler = mark_idx;
            this.stack.clone_frame(mark_idx);
            this.stack.currentFrame().handler = null;
        },
        Continue: ([kidx, frames, arg]) => {
            this.stack.resumeContinuation(frames, this.idx);
            this.idx = kidx;
            this.stack.push(arg);
            this.cmds = this.env.cmds(this.stack.currentFrame().source);
        },
        ReRequest: ([
            kind,
            number,
            args,
            final_index,
            frames,
            current_frame_idx,
        ]) => {
            const [nidx, frame_index] = this.stack.back_again_to_handler(
                frames,
                current_frame_idx,
            );
            this.idx = nidx;
            this.cmds = this.env.cmds(this.stack.currentFrame().source);

            this.stack.push({
                RequestWithContinuation: [
                    kind,
                    number,
                    args,
                    final_index,
                    frames,
                    frame_index,
                ],
            });
        },
        Request: ([kind, number, args]) => {
            const final_index = this.idx;
            const back = this.stack.back_to_handler();
            if (!back) {
                return handleExternalRequest(kind, number, args, this);
                // throw new Error('No handler found for ' + JSON.stringify(kind));
            }
            const [nidx, saved_frames, frame_idx] = back;
            this.idx = nidx;
            this.cmds = this.env.cmds(this.stack.currentFrame().source);

            this.stack.push({
                RequestWithContinuation: [
                    kind,
                    number,
                    args,
                    final_index,
                    saved_frames,
                    frame_idx,
                ],
            });
        },
        FnCall: ([fnid, bindings, arg]) => {
            this.cmds = this.env.anon_fns[fnid][1];
            this.stack.new_frame(this.idx, {
                Fn: [fnid, this.env.anon_fns[fnid][0]],
            });
            this.stack.currentFrame().bindings = bindings;
            this.stack.currentFrame().stack.push(arg);
            this.idx = 0;
        },
        Value: (hash) => {
            this.cmds = this.env.terms[hash][0];
            this.stack.new_frame(this.idx, { Value: hash });
            this.idx = 0;
        },
        HandlePure: () => {
            const [idx1, value] = this.stack.pop_frame();
            this.idx = idx1;
            this.stack.push(value);
            this.cmds = this.env.cmds(this.stack.currentFrame().source);
        },
    };
}

const handleExternalRequest = (kind, number, args, state) => {
    const hash = kind.DerivedId[0];
    if (!state.ffi) {
        throw new Error('State has no "ffi" defined');
    }
    // console.log(hash, number, args);
    if (!state.ffi[hash] || !state.ffi[hash][number]) {
        throw new Error(`No ffi defined for ${hash}`);
    }
    if (typeof state.ffi[hash][number] !== 'function') {
        // throw new Error('async not yet supported');
        const frames = state.stack.drain();
        return {
            FullRequest: [
                hash,
                number,
                args,
                frames,
                state.idx,
                null, // return_type,
            ],
        };
    }
    // TODO translate args
    let value = state.ffi[hash][number](...args);
    if (value == null) {
        value = unit_value();
    }
    // console.log('EXTERANL', hash, number, args, value);
    state.stack.push(value);
    // let constructor_type = self.env.get_ability_type(&kind, number);
    // let concrete_type = match self
    //     .effects
    //     .get(&kind.hash().expect("Not a DerivedId").to_string())
    // {
    //     Some(x) => x,
    //     None => unreachable!(
    //         "No effect found: {:?} - {:?}",
    //         kind.hash(),
    //         self.stack.frames[self.stack.frames.len() - 1].source
    //     ),
    // };
    // let constructor_args = concrete_type.as_tm().unwrap().app_args();
    // let (arg_types, _effects, return_type) = crate::ir_runtime::extract_args(
    //     &constructor_type
    //         .concretize(constructor_args.as_slice(), &Default::default()),
    // );
    // info!(
    //     "Going through args and annotating effects: {:?}",
    //     constructor_type
    // );
    // info!("Going through arg_types: {:?}", arg_types);
    // info!("Going through effects: {:?}", _effects);
    // info!("Going through args: {:?}", args);
    // // for any partial functions, annotate them with the type
    // for (i, arg) in args.iter_mut().enumerate() {
    //     match &**arg {
    //         Value::PartialFnBody(fnid, bindings) => {
    //             *arg = Arc::new(Value::PartialFnBodyWithType(
    //                 *fnid,
    //                 bindings.clone(),
    //                 arg_types[i].clone(),
    //             ))
    //         }
    //         _ => (),
    //     }
    // }
    // // ok folks
    // match ffi.handle_request_sync(&return_type, &kind, number, &args) {
    //     None => {
    //         return Err(Error::Request(FullRequest(
    //             kind,
    //             number,
    //             args,
    //             self.stack.frames.drain(..).collect(),
    //             final_index,
    //             return_type,
    //         )))
    //     }
    //     Some(value) => {
    //         if !crate::check::validate(Default::default(), &return_type, &value)
    //             .is_ok()
    //         {
    //             return Err(Error::InvalidFFI(InvalidFFI(
    //                 kind,
    //                 number,
    //                 Arc::new(value),
    //             )));
    //         }
    //         self.stack.push(Arc::new(value));
    //         return Ok(());
    //     }
    // }
};
