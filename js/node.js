// const data = import('./data/all.json');
// const names = import('./data/all.json.names.json');

import { RuntimeEnv, State, eval_value } from './ir_runtime';
import Trace from './Trace';
import chalk from 'chalk';

import jsonEqual from '@birchill/json-equalish';
// window.jsonEqual = jsonEqual;
import { diff } from './diff';
// window.diff = diff;

const [_, __, arg] = process.argv;

const data = import('../runtime_tests.json');
const names = import('../runtime_tests.json.names.json');

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0);

const run = (data, names) => {
    const env = new RuntimeEnv(data, names);

    const tests = Object.keys(names[0])
        .filter(
            (hash) =>
                names[0][hash][0][0] === 'runtime_tests' &&
                names[0][hash][0].slice(-1)[0].startsWith('t_'),
        )
        .sort((a, b) =>
            cmp(names[0][a][0].join('.'), names[0][b][0].join('.')),
        );

    for (const hash of tests) {
        const name = names[0][hash][0].join('.');
        if (arg != null && name !== arg) {
            continue;
        }
        console.log('Running', name);
        const res = eval_value(env, hash);
        if (res.Boolean !== true) {
            console.log(
                chalk.red(
                    `Unexpected result: ${chalk.blue(JSON.stringify(res))}`,
                ),
            );
        }
    }
};

Promise.all([data, names]).then(([data, names]) => run(data, names));

/*

Ok, what do I need to do here?

First order: eval a term.

How does that happen?
We need
- frame
- ir_exec
- ir_runtime
- pattern
- stack

- extract type args
- convert input args to match the type args
- add_eval (make an eval term)
- make a new state
- run the state

Shorter route (no args)

- make a new state
- run the state

*/
