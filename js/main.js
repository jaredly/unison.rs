// const data = require('../bin/get_random_ints.json');
const data = require('./data/plus_ref.json');

import { RuntimeEnv, State, eval_value } from './ir_runtime';

const env = new RuntimeEnv(data);
const res = eval_value(
    env,
    'fcc1gaq3o7aggcuhkc3prk7ocmfqdhc9g2r3rovpc4na1sb334kng6qa0uo5ve0h47k0q4iurf08ho1jp08pbq8nmeaum7c99heh7k0',
    // 'pi4fuhjsjs1vi1f7o0f7ksgmu11m5v52k9gej14qkd6d7ju3ut5trlkqjbbrj2vrk8k3he5osckpusqgchri5o0rjq4rtg1ujsk1110',
);
console.log('Result:', res);

// const jsToArg = v => {
//     if (typeof v === )
// }

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
