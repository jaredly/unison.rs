// What I imagine.
import unison from './unison';
import makeHandlers from './handlers';

const packed_env = fetch('./data/all.bin').then((r) => r.text());
const names = fetch('./data/all.bin.json').then((r) => r.json());

unison(packed_env, names).then((runtime) => {
    return runtime.run('app_test.counter', [10], makeHandlers(runtime));
});
