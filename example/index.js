// What I imagine.
import unison from './unison';

// Here's how argument conversion works:
// - if it's a builtin, you get the contents
// - if it's a tuple, it gets turned into a string
// - if it's a list, you get the contents
// - if it's an FFI, you get the raw contents
// - otherwise, you get the plain, jsonified dealio
//   NOTE that continuations are expensive ... but I can optimize that later

const packed_env = fetch('./data/all.bin').then((r) => r.text());
const names = fetch('./data/all.bin.json').then((r) => r.json());

unison(packed_env, names).then((runtime) => {
    const mvars = [];

    // all the handlers have to be sync
    // and ... runtime.lambda has a "Sync" and an "async" version. Like
    // if you want to run a lambda with only sync handlers, you can get the sync results

    // runtime.runSync('one.two.three', [arg, arg, arg], handlers);
    // runtime.run('one.two.three', [arg, arg, arg], handlers); // all the handlers have to be sync

    const handlers = {
        // So, for "full" handlers, you get the continuation passed to you.
        // This means you can resume asynchronously, resume multiple times, etc.
        // But this means that running with these handlers will return a promise,
        // instead of being "immediate mode".
        // Should this happen automagically?
        //
        Wait: {
            waitMs: {
                type: 'full',
                handler: (time, k) =>
                    setTimeout(
                        () => runtime.resume(k, null, handlers),
                        time.Nat,
                    ),
            },
        },
        Time: {
            currentTimeStampMs: () => ({ Nat: Date.now() }),
        },
        // Ok, so these handlers are synchronous, which is very good.
        SetTimeout1: {
            setTimeout1: (time, fn) => {
                setTimeout(() => runtime.lambda(fn, null, handlers), time);
            },
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
        MVarAbility: {
            create: (v) => {
                // console.log('create mvar', v);
                mvars.push(v);
                return { Nat: mvars.length - 1 };
            },
            get: (idx) => {
                // console.log('get', idx);
                // console.log('Got', mvars[idx], mvars);
                return mvars[idx];
            },
            set: (idx, v) => {
                // console.log('set', idx, v);
                mvars[idx] = v;
                return null;
            },
        },
    };

    // ummmm how do we know what the value is?
    // theoretically it can be synchronous
    // if we don't have any async handlers
    // so it could be useful to distinguish
    return runtime.run('ffi_7', [null], handlers);
});
