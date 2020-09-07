const newFrame = (source, return_index) => ({
    source: source,
    stack: [],
    marks: [],
    handler: null,
    return_index: return_index,
    bindings: [],
});

const symEq = (a, b) => {
    return a.unique === b.unique;
};

export class Stack {
    constructor(source) {
        this.frames = [newFrame(source, 0)];
    }

    get_vbl(sym, usage) {
        const idx = this.frames[0].bindings.findIndex((b) => symEq(b[0], sym));
        if (idx === -1) {
            throw new Error('sym not found');
        }
        const item = this.frames[0].bindings[idx];
        if (item[1] === usage) {
            this.frames[0].bindings.splice(idx, 1);
            return item[2];
        }
        return item[2];
    }

    new_frame(return_index, source) {
        console.log('->> New Frame', source, return_index);
        this.frames.unshift(newFrame(source, return_index));
    }

    clone_frame(return_index) {
        console.log('->> Clone Frame');
        this.frames.unshift({ ...this.frames[0] });
        this.frames[0].return_index = return_index;
    }

    back_again_to_handler(frames, current_idx) {
        let new_idx = current_idx + 1;
        while (frames[new_idx].handler === null) {
            new_idx += 1;
        }
        this.frames = frames.slice(new_idx);
        let idx = this.frames[0].handler;
        if (idx == null) {
            throw new Error('no handler');
        }
        this.frames[0].handler = null;
        return [idx, new_idx];
    }

    back_to_handler() {
        const frames = [];
        while (this.frames[0].handler === null) {
            frames.push(this.frames.shift());
            if (this.frames.length === 0) {
                return null;
            }
        }
        const current_idx = frames.length - 1;
        frames.push(...this.frames.map((f) => JSON.parse(JSON.stringify(f))));
        const idx = this.frames[0].handler;
        if (idx == null) {
            throw new Error('no handler');
        }
        this.frames[0].handler = null;
        return [idx, frames, current_idx];
    }

    pop_frame() {
        const idx = this.frames[0].return_index;
        const value = this.pop();
        console.log('<<- pop frame', idx, value);
        if (value == null) {
            throw new Error('no return value');
        }
        this.frames.shift();
        return [idx, value];
    }

    push(t) {
        this.frames[0].stack.push(t);
        console.log(
            'push to stack',
            t,
            'stack size',
            this.frames[0].stack.length,
        );
    }

    pop() {
        const t = this.frames[0].stack.pop();
        console.log('pop from stack', t);
        if (t == null) {
            throw new Error(`Popping from the stack, but nothing is there`);
        }
        return t;
    }

    peek() {
        const l = this.frames[0].stack.length;
        if (l > 0) {
            return this.frames[0].stack[l - 1];
        }
        return null;
    }

    pop_to_mark() {
        const mark = this.frames[0].marks.pop();
        if (mark == null) {
            throw new Error('pop to mark');
        }
        while (this.frames[0].stack.length > mark) {
            this.frames[0].stack.pop();
        }
    }

    mark() {
        const ln = this.frames[0].stack.length;
        this.frames[0].marks.push(ln);
    }

    clear_mark() {
        this.frames[0].marks.pop();
    }

    pop_up() {
        const ln = this.frames[0].stack.length;
        this.frames[0].stack.splice(ln - 2, 1);
    }
}
