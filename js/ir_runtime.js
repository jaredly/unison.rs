// Ok folks

import { Stack } from './stack';
import { eval_ir } from './ir_exec';
import { pretty_print } from './pretty_print';

export const eval_value = (env, hash) => {
    const state = new State(env, hash);
    window.trace = state.stack.trace;
    return state.run_to_end();
};

const DEBUG = true;

export class RuntimeEnv {
    // terms: { [key: string]: [Array<IR>, ABTType] };
    // anon_fns: Array<[String, Array<IR>]>;
    // types: { [key: string]: TypeDecl };

    constructor({ terms, anon_fns, types }, names) {
        this.terms = terms;
        this.anon_fns = anon_fns;
        this.types = types;
        // console.log('names', names);
        this.names = names;
    }

    cmds(source) {
        if (source.type === 'term') {
            if (this.terms[source.hash]) return this.terms[source.hash][0];
        }
        if (!this.anon_fns[source.fnid]) {
            throw new Error(`No function: ${JSON.stringify(source)}`);
        }
        return this.anon_fns[source.fnid][1];
    }
}

export class State {
    constructor(env, hash) {
        if (!env.terms[hash]) {
            for (let k of Object.keys(env.terms)) {
                if (k.startsWith(hash)) {
                    hash = k;
                    break;
                }
            }
            if (!env.terms[hash]) {
                throw new Error(`Hash not found ${hash}`);
            }
        }
        this.trace = {};
        this.cmds = env.terms[hash][0];
        this.idx = 0;
        this.env = env;
        this.stack = new Stack({ type: 'term', hash });
    }

    pretty_print(value) {
        return pretty_print(this.env.names, value);
    }

    run_to_end() {
        // console.log(this.cmds);
        this.run();
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
            this.stack.trace[this.stack.currentFrame().traceId].events.push({
                type: 'ir',
                idx,
                cmd,
                start,
                // ret,
                // end: Date.now(),
            });
            const ret = eval_ir(cmd, this);
            if (ret) {
                this.stack.trace[this.stack.currentFrame().traceId].events.push(
                    {
                        type: 'ret',
                        ret,
                    },
                );
                this.handle_ret(ret);
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
        if (this.rets[typ] == null) {
            throw new Error(
                `Cannot handle ret ${JSON.stringify(ret)} : ${typ}`,
            );
        }
        return this.rets[typ](ret[typ]);
    }

    rets = {
        Handle: (mark_idx) => {
            this.idx += 1;
            if (this.stack.currentFrame().handler) {
                throw new Error('Stack frame already has a handler');
            }
            this.stack.currentFrame().handler = mark_idx;
            // const ln = this.stack.frames.length;
            this.stack.clone_frame(mark_idx);
            this.stack.currentFrame().handler = null;
        },
        Continue: ([kidx, frames, arg]) => {
            // TODO faster please
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
            const [
                nidx,
                saved_frames,
                frame_idx,
            ] = this.stack.back_to_handler();
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
                type: 'Fn',
                fnid,
                hash: this.env.anon_fns[fnid][0],
            });
            this.stack.currentFrame().bindings = bindings;
            this.stack.currentFrame().stack.push(arg);
            this.idx = 0;
        },
        Value: (hash) => {
            this.cmds = this.env.terms[hash][0];
            this.stack.new_frame(this.idx, { type: 'term', hash });
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
