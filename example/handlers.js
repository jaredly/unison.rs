export default (runtime) => {
    const mvars = [];

    // all the handlers have to be sync
    // and ... runtime.lambda has a "Sync" and an "async" version. Like
    // if you want to run a lambda with only sync handlers, you can get the sync results

    // runtime.runSync('one.two.three', [arg, arg, arg], handlers);
    // runtime.run('one.two.three', [arg, arg, arg], handlers); // all the handlers have to be sync

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
                document.body.appendChild(elements[idx]);
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
                    // TODO figure out how to make a MouseEvent
                    // probably just pass around a tuple for the moment?
                    // I don't think I properly wrap tuples yet.
                    runtime.lambda(handler, evt.clientY, handlers);
                });
            },
            addKeyEventListener: (idx, evt, handler) => {
                elements[idx].addEventListener(evt, (evt) => {
                    // TODO figure out how to make a MouseEvent
                    // probably just pass around a tuple for the moment?
                    // I don't think I properly wrap tuples yet.
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
