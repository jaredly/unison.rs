const js = import('./node_modules/unison_wasm/unison_wasm.js');
import data from 'raw-loader!../bin/get_random_ints.bin';
js.then((js) => {
    console.log('loading...');
    js.load(data);
    console.log('loaded...');
    console.log(
        '<- got back',
        js.eval_fn(
            'gdm03spbhhqlbh035rh42udv3bsirtpg76n9hosgc7igj9mjp68115urcdbfdbdn8r1c38anrf7blj1qf9mpi42dqvgu333rp41d9k8',
            [0, 10],
        ),
    );
});
