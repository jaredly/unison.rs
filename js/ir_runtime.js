// Ok folks

import { Stack } from './stack';
import { eval_ir } from './ir_exec';
import { pretty_print } from './pretty_print';

export const eval_value = (env, hash) => {
    const state = new State(env, hash);
    return state.run_to_end();
};

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
            // console.log(`- (${this.idx})`, this.cmds[this.idx]);
            const ret = eval_ir(this.cmds[this.idx], this);
            if (ret) {
                this.handle_ret(ret);
            }
            this.handle_tail();
        }
    }

    handle_tail() {
        while (this.idx >= this.cmds.length) {
            if (this.stack.frames.length > 1) {
                const [idx1, value] = this.stack.pop_frame();
                this.idx = idx1;
                this.stack.push(value);
                this.cmds = this.env.cmds(this.stack.frames[0].source);
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
            if (this.stack.frames[0].handler) {
                throw new Error('Stack frame already has a handler');
            }
            this.stack.frames[0].handler = mark_idx;
            const ln = this.stack.frames.length;
            this.stack.clone_frame(mark_idx);
            this.stack.frames[0].handler = null;
        },
        Continue: ([kidx, frames, arg]) => {
            const last = frames.length - 1;
            frames[last].return_index = this.idx;
            frames.push(...this.stack.frames);
            this.stack.frames = frames;
            this.idx = kidx;
            this.stack.push(arg);
            this.cmds = this.env.cmds(this.stack.frames[0].source);
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
            this.cmds = this.env.cmds(this.stack.frames[0].source);

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
            this.cmds = this.env.cmds(this.stack.frames[0].source);

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
            this.stack.frames[0].bindings = bindings;
            this.stack.frames[0].stack.push(arg);
            this.idx = 0;
        },
        Value: (hash) => {
            this.cmds = this.env.terms[hash][0];
            this.stack.new_frame(this.idx, { type: 'value', hash });
            this.idx = 0;
        },
        HandlePure: () => {
            const [idx1, value] = this.stack.pop_frame();
            this.idx = idx1;
            this.stack.push(value);
            this.cmds = this.env.cmds(this.stack.frames[0].source);
        },
    };
}
