export default (runtime, root) => {
    const mvars = [];

    const elements = [];

    const handlers = {
        Time: {
            currentTimeStampMs: () => ({ Nat: Date.now() }),
        },
        Console: {
            log: (value) => {
                if (Array.isArray(value)) {
                    console.log(...value);
                } else {
                    console.log(value);
                }
            },
            warn: (value) => {
                if (Array.isArray(value)) {
                    console.warn(...value);
                } else {
                    console.warn(value);
                }
            },
            error: (value) => {
                if (Array.isArray(value)) {
                    console.error(...value);
                } else {
                    console.error(value);
                }
            },
        },
        'app_test.Document': {
            getElementById: (x) => {
                throw new Error('nope');
            },
            createElement: (tag) => {
                elements.push(document.createElement(tag));
                return { Nat: elements.length - 1 };
            },
            addToBody: (idx) => {
                root.appendChild(elements[idx]);
            },
            setAttribute: (idx, attr, value) => {
                elements[idx].setAttribute(attr, value);
            },
            setTextContent: (idx, text) => {
                elements[idx].textContent = text;
            },
            appendChild: (one, two) => {
                elements[one].appendChild(elements[two]);
            },
            addMouseEventListener: (idx, evt, handler) => {
                console.log('event listener', idx, handler, elements);
                elements[idx].addEventListener(evt, (evt) => {
                    runtime.lambda(handler, evt.clientY, handlers);
                });
            },
            addKeyEventListener: (idx, evt, handler) => {
                elements[idx].addEventListener(evt, (evt) => {
                    runtime.lambda(handler, evt.key, handlers);
                });
            },
            getValue: (idx) => elements[idx].value,
            setValue: (idx, text) => (elements[idx].value = text),
            getIntAttribute: (idx, attr) =>
                elements[idx].getAttribute(attr) | 0,
            getTextAttribute: (idx, attr) =>
                elements[idx].getAttribute(attr).toString(),
            getBoolAttribute: (idx, attr) => !!elements[idx].getAttribute(attr),
        },
        'app_test.MVar': {
            create: (v) => {
                mvars.push(v);
                return { Nat: mvars.length - 1 };
            },
            get: (idx) => {
                return mvars[idx];
            },
            set: (idx, v) => {
                mvars[idx] = v;
                return null;
            },
        },
    };

    return handlers;
};
