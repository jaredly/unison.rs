// What I imagine.
import unison, { fetch } from './unison';
import makeHandlers from './handlers';

// fetch('./data/all.bin').then(
//     (runtime) => {
//             runtime.run('app_test.counter', [10], makeHandlers(runtime))
//     },
//     (err) => console.error(err),
// );

fetch('./data/get_random_ints.bin').then(
    (runtime) => {
        window.runtime = runtime;
        console.log('Ok running');
        console.log('Result', runtime.runSync('get_random_ints', [10, 10], {}));
    },
    (err) => console.error(err),
);
