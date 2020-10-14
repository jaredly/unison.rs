// const data = import('./data/all.json');
// const names = import('./data/all.json.names.json');

import { RuntimeEnv, State, eval_value } from './ir_runtime';
import Trace from './Trace';

import jsonEqual from '@birchill/json-equalish';
// window.jsonEqual = jsonEqual;
import { diff } from './diff';
// window.diff = diff;

const data = import('../runtime_tests.json');
const names = import('../runtime_tests.json.names.json');
// const data = fetch('./all.json').then((r) => r.json());
// const trace = fetch('./trace_rust.json').then((r) => r.json());

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0);

Promise.all([data, names]).then(([data, names]) => {
    // window.names = names;
    // window.data = data;

    const env = new RuntimeEnv(data, names);

    const tests = Object.keys(names[0])
        .filter((hash) => names[0][hash][0][0] === 'runtime_tests')
        .sort((a, b) =>
            cmp(names[0][a][0].join('.'), names[0][b][0].join('.')),
        );

    // const hash =
    //     '6bqkgtjhf9mbtlclp9mg3at7jcohidcn1su5u6bhh6tc5o83evov8qncm635ou8g6rdv4o1trv0bftppdkhn5g2k4pbdhjf78dm86to';
    // console.log('ok');
    // const hash = 'm3sai7lcac';

    for (const hash of tests) {
        console.log('Running', names[0][hash]);
        try {
            const start = Date.now();
            const res = eval_value(env, hash);
            console.log(`${Date.now() - start}ms`);
            console.log('Result:', JSON.stringify(res));
        } catch (err) {
            throw err;
        }
    }
});

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
