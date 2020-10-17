import clone from 'clone-deep';

const newFrame = (source, return_index, trace_id) => ({
    source: source,
    stack: [],
    marks: [],
    handler: null,
    return_index: return_index,
    bindings: [],
    trace_id,
});

const symEq = (a, b) => {
    return a.unique === b.unique;
};

/* istanbul ignore next */
const showSource = (source) => {
    if (source.Fn) {
        const [fnid, hash] = source.Fn;
        return `${hash.slice(0, 10)} : Fn(${fnid})`;
    }
    if (source.Value) {
        return `${source.Value.slice(0, 10)} : Value`;
    }
    return source;
};

const DEBUG = false;

// What are we tracing?
// - stack adds & pops probably
// - function return arguments?
// - hmmm
// - maybe:
//   - [ts: frame start, source]
//   - [push/pop]
//   - [new_frame (trace_id)]
//   - [clone_frame (trace_id)]
//   - IR (idx)
// - OK I think I'm in good shape?

export class Stack {
    constructor(source) {
        this._frames = [];
        // this.trace = [];
        this.new_frame(0, source);
    }

    bind(binding) {
        this._frames[0].bindings.unshift(binding);
    }

    bindLast(binding) {
        this._frames[0].bindings.push(binding);
    }

    currentFrame() {
        return this._frames[0];
    }

    isLastFrame() {
        return this._frames.length <= 1;
    }

    resumeContinuation(frames, returnIdx) {
        frames = frames.map((f) => ({ ...f }));
        const last = frames.length - 1;
        frames[last].return_index = returnIdx;
        /* istanbul ignore next */
        if (this.trace) {
            frames.forEach((f) => {
                const prev = f.trace_id;
                const tid = this.trace.length;
                f.trace_id = tid;
                this.trace[f.trace_id] = {
                    source: { type: 'clone', tid: prev },
                    frame: f.source,
                    start: Date.now(),
                    events: [],
                };
            });
        }
        frames.push(...this._frames);
        this._frames = frames;
    }

    get_vbl(sym, usage) {
        const idx = this._frames[0].bindings.findIndex((b) => symEq(b[0], sym));
        /* istanbul ignore next */
        if (idx === -1) {
            throw new Error('sym not found');
        }
        const item = this._frames[0].bindings[idx];
        if (item[1] === usage) {
            this._frames[0].bindings.splice(idx, 1);
            return item[2];
        }
        return item[2];
    }

    new_frame(return_index, source) {
        /* istanbul ignore next */
        if (DEBUG) {
            console.log(
                `->> ${this._frames.length} New Frame : `,
                showSource(source),
                return_index,
            );
        }
        const prevId = this._frames.length ? this._frames[0].trace_id : null;
        /* istanbul ignore next */
        const tid = this.trace ? this.trace.length : null;
        this._frames.unshift(newFrame(source, return_index, tid));
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id] = {
                start: Date.now(),
                frame: source,
                source: { tid: prevId },
                events: [],
            };

            if (this._frames.length > 1) {
                this.trace[this._frames[1].trace_id].events.push({
                    NewFrame: this._frames[0].trace_id,
                });
            }
        }
    }

    clone_frame(return_index) {
        /* istanbul ignore next */
        if (DEBUG) {
            console.log('->> Clone Frame');
        }
        const old_tid = this._frames[0].trace_id;
        this._frames.unshift(clone(this._frames[0]));
        this._frames[0].return_index = return_index;
        /* istanbul ignore next */
        if (this.trace) {
            this._frames[0].trace_id = this.trace.length;
            this.trace[this._frames[0].trace_id] = {
                start: Date.now(),
                frame: this._frames[0].source,
                source: { type: 'clone', tid: this._frames[1].trace_id },
                events: [],
            };
            this.trace[old_tid].events.push({
                CloneFrame: this._frames[0].trace_id,
            });
        }
    }

    back_again_to_handler(frames, current_idx) {
        const old_tid = this._frames[0].trace_id;
        let new_idx = current_idx + 1;
        while (frames[new_idx].handler == null) {
            new_idx += 1;
        }
        // TODO how to account for this "going back"?
        // Add an event to all the frames being like "resuming"?
        this._frames = clone(frames.slice(new_idx));
        let idx = this._frames[0].handler;
        /* istanbul ignore next */
        if (idx == null) {
            throw new Error('no handler');
        }
        this._frames[0].handler = null;
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push('HandleAgain');
            this.trace[old_tid].events.push({
                JumpBack: this._frames[0].trace_id,
            });
        }
        return [idx, new_idx];
    }

    back_to_handler() {
        let has_handler = false;
        for (let i = 0; i < this._frames.length; i++) {
            if (this._frames[i].handler != null) {
                has_handler = true;
                break;
            }
        }
        if (!has_handler) {
            return null;
        }
        const old_tid = this._frames[0].trace_id;
        const frames = [];
        while (this._frames[0].handler == null) {
            /* istanbul ignore next */
            if (this.trace) {
                this.trace[this._frames[0].trace_id].events.push('Pause');
            }
            frames.push(this._frames.shift());
            if (this._frames.length === 0) {
                return null;
            }
        }
        const current_idx = frames.length - 1;
        frames.push(...clone(this._frames));
        const idx = this._frames[0].handler;
        /* istanbul ignore next */
        if (idx == null) {
            throw new Error('no handler');
        }
        this._frames[0].handler = null;
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push('Handle');
            this.trace[old_tid].events.push({
                JumpBack: this._frames[0].trace_id,
            });
        }
        return [idx, frames, current_idx];
    }

    pop_frame() {
        const idx = this._frames[0].return_index;
        const value = this.pop();
        /* istanbul ignore next */
        if (DEBUG) {
            console.log(
                `<<- ${this._frames.length} Pop Frame : ${idx} - ${showSource(
                    this._frames[1].source,
                )}`,
                value,
            );
        }
        /* istanbul ignore next */
        if (value == null) {
            throw new Error('no return value');
        }
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].end = Date.now();
        }
        this._frames.shift();
        return [idx, value];
    }

    push(t) {
        this._frames[0].stack.push(t);
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push({
                Push: clone(t),
            });
        }
        /* istanbul ignore next */
        if (DEBUG) {
            console.log(
                'push to stack',
                t,
                'stack size',
                this._frames[0].stack.length,
            );
        }
    }

    pop() {
        const t = this._frames[0].stack.pop();
        // TODO do I need the value here? not really
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push({
                Pop: clone(t),
            });
        }
        /* istanbul ignore next */
        if (DEBUG) {
            console.log('pop from stack', t);
        }
        /* istanbul ignore next */
        if (t == null) {
            console.log(this._frames);
            throw new Error(`Popping from the stack, but nothing is there`);
        }
        return t;
    }

    peek() {
        const l = this._frames[0].stack.length;
        if (l > 0) {
            return this._frames[0].stack[l - 1];
        }
        return null;
    }

    pop_to_mark() {
        const mark = this._frames[0].marks.pop();
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push({
                PopToMark: mark,
            });
        }
        /* istanbul ignore next */
        if (mark == null) {
            throw new Error('pop to mark');
        }
        while (this._frames[0].stack.length > mark) {
            this._frames[0].stack.pop();
        }
    }

    mark() {
        const ln = this._frames[0].stack.length;
        this._frames[0].marks.push(ln);
    }

    clear_mark() {
        this._frames[0].marks.pop();
    }

    pop_up() {
        const ln = this._frames[0].stack.length;
        this._frames[0].stack.splice(ln - 2, 1);
        /* istanbul ignore next */
        if (this.trace) {
            this.trace[this._frames[0].trace_id].events.push('PopUp');
        }
    }
}
