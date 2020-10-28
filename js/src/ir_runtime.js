// Ok folks

import clone from 'clone-deep';
import { Stack } from './stack';
import { eval_ir } from './ir_exec';
import { pretty_print } from './pretty_print';
import { State } from './state';

export const eval_value = (env, hash, debug = {}) => {
    const state = new State(env, {}, debug);
    state.loadHash(hash);
    // window.trace = state.stack.trace;
    return state.run_to_end();
};

const DEBUG = true;

export class RuntimeEnv {
    constructor({ terms, anon_fns, types }, names) {
        this.terms = terms;
        this.anon_fns = anon_fns;
        this.types = types;
        this.names = names;
        this.evals = 0;
    }

    eval(hash, ffi, debug = {}) {
        const state = new State(this, ffi, debug);
        state.loadHash(hash);
        return state.run_to_end();
    }

    resume(kont, arg, ffi, debug = {}) {
        const state = new State(this, ffi, debug);
        state.fullResume(kont, arg);
        return state.run_to_end();
    }

    addEval(hash, args) {
        const newHash = `<eval-${this.evals++}>`;
        const current = this.terms[hash];
        const cmds = [{ Value: { Ref: { DerivedId: [hash, 0, 1] } } }];
        // const [_, __, typ] = extractArgs(current[1]);
        const typ = 'dynamically added folks';
        args.forEach((arg) => {
            cmds.push({ Value: arg });
            cmds.push('Call');
        });
        this.terms[newHash] = [cmds, typ];
        return newHash;
    }

    cmds(source) {
        if (source.Value) {
            if (this.terms[source.Value]) return this.terms[source.Value][0];
        }
        /* istanbul ignore next */
        if (!this.anon_fns[source.Fn[0]]) {
            throw new Error(`No function: ${JSON.stringify(source)}`);
        }
        return this.anon_fns[source.Fn[0]][1];
    }
}
