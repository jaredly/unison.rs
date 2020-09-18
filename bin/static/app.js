// What I imagine.

const ws = new WebSocket(`ws://${location.host}/reload-notifier`);
console.log('setting up ws');
ws.addEventListener('message', (evt) => {
    console.log('got evt!', evt);
    const head = evt.data;
    console.log('loading', head);
    window
        .loadUnison(
            '/build/' + head + '/.app_test.app',
            '/build/' + head + '/.app_test.app/names',
        )
        .then((runtime) => {
            console.log('Reloading!');
            document.body.innerHTML = '';
            runtime.run(
                'app_test.app',
                [null],
                window.makeDefaultHandlers(runtime),
            );
        });
});

// window.loadUnison('./data/counter_new.bin').then((runtime) => {
//     console.log('ok');
//     runtime.run('app_test.counter', [10], window.makeDefaultHandlers(runtime));
// });
