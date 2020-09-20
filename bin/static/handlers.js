// Good news
window.makeAbilityHandlers = (runtime, root) => {
    const mvars = [];

    const elements = [];

    const handlers = {
        handlers: {
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
                getBoolAttribute: (idx, attr) =>
                    !!elements[idx].getAttribute(attr),
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
        },
        hashes: {
            'app_test.Document': {
                hash:
                    'rvocknbja40ovcaj9s10jl338kcbug2ot0rueht6ntqlfhobpg7ors79ujbrfoskbv62nu824rnn5h6f1cr4c86drpe1jbpdubi9ego',
                idxs: {
                    getElementById: 1,
                    createElement: 5,
                    addToBody: 2,
                    setAttribute: 10,
                    setTextContent: 8,
                    appendChild: 7,
                    addMouseEventListener: 4,
                    addKeyEventListener: 6,
                    getValue: 3,
                    setValue: 9,
                    getIntAttribute: 11,
                    getTextAttribute: 0,
                    getBoolAttribute: 12,
                },
            },
            'app_test.MVar': {
                hash:
                    'pi31tckpdqj3kko9bs8svcfmgidvgnfi397ng5k38o830gu3valts9ldts322cf6kfnmmp967ctovlnb2996l3pjkcp0d8hfd6febeg',
                idxs: { create: 0, get: 1, set: 2 },
            },
        },
    };
    return handlers;
};
