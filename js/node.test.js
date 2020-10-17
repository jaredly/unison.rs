import { RuntimeEnv, eval_value } from './src/ir_runtime';
import fs from 'fs';
import compare from './src/compare';

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0);

const data = JSON.parse(
    fs.readFileSync(__dirname + '/data/runtime_tests.json', 'utf8'),
);
const names = JSON.parse(
    fs.readFileSync(__dirname + '/data/runtime_tests.json.names.json', 'utf8'),
);

const debug = {};
if (process.env.DEBUG) {
    process.env.DEBUG.split(',').forEach((n) => (debug[n] = true));
}

const env = new RuntimeEnv(data, names);

describe('runtime tests', () => {
    const tests = Object.keys(names[0])
        .filter(
            (hash) =>
                names[0][hash][0][0] === 'runtime_tests' &&
                names[0][hash][0].slice(-1)[0].startsWith('t_'),
        )
        .sort((a, b) =>
            cmp(names[0][a][0].join('.'), names[0][b][0].join('.')),
        );
    const groups = {};
    tests.forEach((hash) => {
        const name = names[0][hash][0][1];
        if (!groups[name]) {
            groups[name] = [];
        }
        groups[name].push(hash);
    });

    for (const name of Object.keys(groups).sort()) {
        describe(name, () => {
            groups[name].forEach((hash) => {
                const name = names[0][hash][0].join('.');
                test(name, () => {
                    const res = env.eval(hash, {}, debug);
                    expect(res).toEqual({ Boolean: true });
                });
            });
        });
    }
});

describe('ffi tests', () => {
    const tests = Object.keys(names[0])
        .filter(
            (hash) =>
                names[0][hash][0][0] === 'ffi_tests' &&
                names[0][hash][0].slice(-1)[0].startsWith('t_'),
        )
        .sort((a, b) =>
            cmp(names[0][a][0].join('.'), names[0][b][0].join('.')),
        );
    const groups = {};
    tests.forEach((hash) => {
        const name = names[0][hash][0][1];
        if (!groups[name]) {
            groups[name] = [];
        }
        groups[name].push(hash);
    });

    const ffi = {
        // MVar
        itn4m9gem5cdsu2ira2nv7vnr1vnp8uabqeveccqqqabvoff8qbicuejpno80van23r5mjh1sdjr0m7uhggb6tjc4hk6e332p5rdako: {
            // create
            0: (value) => ({ FFI: value }),
            // get
            1: (ffi) => ffi.FFI,
            // set
            2: (ffi, value) => {
                ffi.FFI = value;
            },
        },
        '2s2bg310fmfsqnt64pifaib59ige9jc3dc9hr5j43eqv0mbg9q7a5gdkq51563bcot8skugkvbnea8ke44abd28p3inbho57h4m7mf0': {
            0: (value) => {
                if (!value) {
                    throw new Error('bad news');
                }
            },
            1: {
                if(value) {
                    throw new Error('bad news');
                },
            },
            2: (a, b) => {
                // STOPSHIP do real compare
                if (compare(a, b) != 0) {
                    throw new Error(
                        `Expected ${JSON.stringify(
                            a,
                        )} to equal ${JSON.stringify(b)}`,
                    );
                }
            },
        },
        // Time
        aai3v4mu9kmdrdonf06a6dfu2b938miqt89psejttkv95or04ks6v89aqi3rq6o6dcl9ngqq9la98dk5vpq9hpc0and6buoik4a8h68: {
            0: (tid) => clearTimeout(tid),
            1: () => ({ Nat: Date.now() }),
            2: (iid) => clearInterval(iid), // STOPSHIP unwrap ffi?
            3: (fn, interval) => {
                throw new Error('lambdas not yet supported');
            },
            4: (fn, timeout) => {
                throw new Error('lambdas not yet supported');
            },
            5: {
                fn: (fn, timeout) => {
                    throw new Error('async not yet supported');
                },
            },
        },
    };

    for (const name of Object.keys(groups).sort()) {
        describe(name, () => {
            groups[name].forEach((hash) => {
                const name = names[0][hash][0].join('.');
                test(name, () => {
                    const evalHash = env.addEval(hash, [{ Nat: 1 }]);
                    const res = env.eval(evalHash, ffi, debug);
                    // const res = env.eval(hash, debug);
                    expect(res).toEqual({ Boolean: true });
                });
            });
        });
    }
});
