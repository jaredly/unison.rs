const data = import('./data/all.json');
const names = import('./data/all.json.names.json');

import { RuntimeEnv, State, eval_value } from './ir_runtime';

Promise.all([data, names]).then(([data, names]) => {
    const env = new RuntimeEnv(data, names);

    Object.keys(names[0]).forEach((hash) => {
        names[0][hash].sort((a, b) => a.length - b.length);
        const main = names[0][hash][0];
        if (main.slice(-1)[0] === 'test' && !main.some((x) => x[0] === '_')) {
            console.log(names[0][hash]);
            console.log(main.join('.'));
            const start = Date.now();
            const res = eval_value(env, hash);
            console.log(`${Date.now() - start}ms`);
            console.log('Result:', JSON.stringify(res));
        }
    });

    // const res = eval_value(env, '5rjk153mh1');
    // // 'n674nqesdnte3k2gv52ss50ubkbg6b25ijbivqton6iopqfvdh39e59umlje0c288nje65bipl3k1n5bvp3nbuvb11qu7t0bl0hdn00',
    // // 'fcc1gaq3o7aggcuhkc3prk7ocmfqdhc9g2r3rovpc4na1sb334kng6qa0uo5ve0h47k0q4iurf08ho1jp08pbq8nmeaum7c99heh7k0',
    // // 'pi4fuhjsjs1vi1f7o0f7ksgmu11m5v52k9gej14qkd6d7ju3ut5trlkqjbbrj2vrk8k3he5osckpusqgchri5o0rjq4rtg1ujsk1110',
    // console.log(`${Date.now() - start}ms`);
    // console.log('Result:', res);
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
