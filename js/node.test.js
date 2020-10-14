import { RuntimeEnv, State, eval_value } from './ir_runtime';
// import chalk from 'chalk';
import fs from 'fs';

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
                    const res = eval_value(env, hash, debug);
                    expect(res).toEqual({ Boolean: true });
                });
            });
        });
    }
    // tests.forEach((hash) => {
    //     const name = names[0][hash][0].join('.');
    //     test(name, () => {
    //         const res = eval_value(env, hash, debug);
    //         expect(res).toEqual({ Boolean: true });
    //     });
    // });
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

    for (const name of Object.keys(groups).sort()) {
        describe(name, () => {
            groups[name].forEach((hash) => {
                const name = names[0][hash][0].join('.');
                test(name, () => {
                    const res = eval_value(env, hash, debug);
                    expect(res).toEqual({ Boolean: true });
                });
            });
        });
    }
});
