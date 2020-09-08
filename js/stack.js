const newFrame = (source, return_index, traceId) => ({
    source: source,
    stack: [],
    marks: [],
    handler: null,
    return_index: return_index,
    bindings: [],
    traceId,
});

const symEq = (a, b) => {
    return a.unique === b.unique;
};

const showSource = (source) => {
    if (source.type === 'Fn') {
        return `${source.hash.slice(0, 10)} : Fn(${source.fnid})`;
    }
    if (source.type === 'term') {
        return `${source.hash.slice(0, 10)} : Value`;
    }
    return source;
};

const DEBUG = true;

// What are we tracing?
// - stack adds & pops probably
// - function return arguments?
// - hmmm
// - maybe:
//   - [ts: frame start, source]
//   - [push/pop]
//   - [new_frame (traceId)]
//   - [clone_frame (traceId)]
//   - IR (idx)
// - OK I think I'm in good shape?

export class Stack {
    constructor(source) {
        this.traceCounter = 0;
        this._frames = [];
        this.trace = {};
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
        frames = JSON.parse(JSON.stringify(frames));
        const last = frames.length - 1;
        frames[last].return_index = returnIdx;
        frames.forEach((f) => {
            const prev = f.traceId;
            f.traceId = this.traceCounter++;
            this.trace[f.traceId] = {
                source: { type: 'clone', tid: prev },
                frame: f.source,
                start: Date.now(),
                events: [],
            };
        });
        frames.push(...this._frames);
        this._frames = frames;
    }

    get_vbl(sym, usage) {
        const idx = this._frames[0].bindings.findIndex((b) => symEq(b[0], sym));
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
        if (DEBUG) {
            console.log(
                `->> ${this._frames.length} New Frame : `,
                showSource(source),
                return_index,
            );
        }
        const prevId = this._frames.length ? this._frames[0].traceId : null;
        this._frames.unshift(
            newFrame(source, return_index, this.traceCounter++),
        );
        this.trace[this._frames[0].traceId] = {
            start: Date.now(),
            frame: source,
            source: { tid: prevId },
            events: [],
        };
        if (this._frames.length > 1) {
            this.trace[this._frames[1].traceId].events.push({
                type: 'new_frame',
                traceId: this._frames[0].traceId,
            });
        }
    }

    clone_frame(return_index) {
        if (DEBUG) {
            console.log('->> Clone Frame');
        }
        this._frames.unshift({ ...this._frames[0] });
        this._frames[0].return_index = return_index;
        this._frames[0].traceId = this.traceCounter++;
        this.trace[this._frames[0].traceId] = {
            start: Date.now(),
            frame: this._frames[0].source,
            source: { type: 'clone', tid: this._frames[1].traceId },
            events: [],
        };
    }

    back_again_to_handler(frames, current_idx) {
        let new_idx = current_idx + 1;
        while (frames[new_idx].handler === null) {
            new_idx += 1;
        }
        // TODO how to account for this "going back"?
        // Add an event to all the frames being like "resuming"?
        this._frames = frames.slice(new_idx);
        let idx = this._frames[0].handler;
        if (idx == null) {
            throw new Error('no handler');
        }
        this._frames[0].handler = null;
        return [idx, new_idx];
    }

    back_to_handler() {
        const frames = [];
        while (this._frames[0].handler === null) {
            frames.push(this._frames.shift());
            if (this._frames.length === 0) {
                return null;
            }
        }
        const current_idx = frames.length - 1;
        frames.push(...this._frames.map((f) => JSON.parse(JSON.stringify(f))));
        const idx = this._frames[0].handler;
        if (idx == null) {
            throw new Error('no handler');
        }
        this._frames[0].handler = null;
        return [idx, frames, current_idx];
    }

    pop_frame() {
        const idx = this._frames[0].return_index;
        const value = this.pop();
        if (DEBUG) {
            console.log(
                `<<- ${this._frames.length} Pop Frame : ${idx} - ${showSource(
                    this._frames[1].source,
                )}`,
                value,
            );
        }
        if (value == null) {
            throw new Error('no return value');
        }
        this.trace[this._frames[0].traceId].end = Date.now();
        this._frames.shift();
        return [idx, value];
    }

    push(t) {
        this._frames[0].stack.push(t);
        this.trace[this._frames[0].traceId].events.push({
            type: 'push',
            value: t,
        });
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
        this.trace[this._frames[0].traceId].events.push({
            type: 'pop',
            value: t,
        });
        if (DEBUG) {
            console.log('pop from stack', t);
        }
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
        this.trace[this._frames[0].traceId].events.push({
            type: 'pop_to_mark',
            mark,
        });
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
        this.trace[this._frames[0].traceId].events.push({ type: 'pop_up' });
    }
}
