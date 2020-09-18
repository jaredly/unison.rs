// What I imagine.

const ws = new WebSocket(`ws://${location.host}/reload-notifier`);

const getTerms = (hash, ns) =>
    fetch(`/terms/${hash}/${ns.join('.')}`).then((r) => r.json());
let latestHash = null;

console.log('setting up ws');
ws.addEventListener('message', (evt) => {
    console.log('got evt!', evt);
    const head = evt.data;
    latestHash = head;
    console.log('loading', head);
    main();
});

const main = () => {
    if (!latestHash) {
        return;
    }
    const hash = window.location.hash.slice(1);
    if (hash.startsWith('render:')) {
        render(latestHash, hash.slice('render:'.length));
    } else {
        showTerms(latestHash, hash ? hash.split('.') : []);
    }
};

const showTerms = (head, ns) => {
    document.body.innerHTML = '';
    getTerms(head, ns).then(([terms, children]) => {
        children.sort().forEach((child) => {
            const button = document.createElement('button');
            button.onclick = () => {
                window.location.hash = '#' + ns.concat(child).join('.');
            };
            Object.assign(button.style, {
                display: 'block',
                background: 'none',
                textDecoration: 'underline',
                cursor: 'pointer',
                border: 'none',
                color: 'blue',
                padding: '0',
                margin: '8px 0',
            });
            button.textContent = child + '/';
            document.body.appendChild(button);
        });
        terms.sort().forEach((term) => {
            const button = document.createElement('button');
            button.textContent = term;
            Object.assign(button.style, {
                display: 'block',
            });
            button.onclick = () => {
                window.location.hash = '#render:' + ns.concat(term).join('.');
            };
            document.body.appendChild(button);
        });
    });
};

const render = (hash, term) => {
    console.log('rendering', hash, term);
    window
        .loadUnison(`/build/${hash}/.${term}`, `/build/${hash}/.${term}/names`)
        .then((runtime) => {
            console.log('Reloading!');
            document.body.innerHTML = '';
            runtime.run(term, [null], window.makeDefaultHandlers(runtime));
        });
};

window.onhashchange = main;
