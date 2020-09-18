// What I imagine.
import { fetch } from './unison';
import makeHandlers from './handlers';

window.loadUnison = fetch;
window.makeDefaultHandlers = makeHandlers;

// This is just to make webpack-dev-server refresh for me :D
// import './data/counter_new.bin.json';

// fetch('./data/counter_new.bin').then(
//     (runtime) => {
//         runtime.run('app_test.counter', [10], makeHandlers(runtime));
//     },
//     (err) => console.error(err),
// );

// TODO: this is super slow
// fetch('./data/get_random_ints.bin').then(
//     (runtime) => {
//         window.runtime = runtime;
//         console.log('Ok running');
//         console.log('Result', runtime.runSync('get_random_ints', [10, 10], {}));
//     },
//     (err) => console.error(err),
// );
