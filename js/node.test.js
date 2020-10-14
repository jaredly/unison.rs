import { RuntimeEnv, State, eval_value } from './ir_runtime';
// import chalk from 'chalk';
import fs from 'fs';

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0);

const data = JSON.parse(
    fs.readFileSync(__dirname + '/../runtime_tests.json', 'utf8'),
);
const names = JSON.parse(
    fs.readFileSync(__dirname + '/../runtime_tests.json.names.json', 'utf8'),
);

const debug = {};
if (process.env.DEBUG) {
    process.env.DEBUG.split(',').forEach((n) => (debug[n] = true));
}

const env = new RuntimeEnv(data, names);

const tests = Object.keys(names[0])
    .filter(
        (hash) =>
            names[0][hash][0][0] === 'runtime_tests' &&
            names[0][hash][0].slice(-1)[0].startsWith('t_'),
    )
    .sort((a, b) => cmp(names[0][a][0].join('.'), names[0][b][0].join('.')));

tests.forEach((hash) => {
    const name = names[0][hash][0].join('.');
    test(name, () => {
        const res = eval_value(env, hash, debug);
        expect(res).toEqual({ Boolean: true });
    });
});
