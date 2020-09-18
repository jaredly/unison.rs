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

window.argsData = null;

// const renderArgs =

const showResult = (result) => {
    console.log('--- RESULT ---');
    console.log(result);
    const node = document.createElement('div');
    node.textContent = JSON.stringify(result);
    Object.assign(node.style, {
        display: 'block',
        whiteSpace: 'pre-wrap',
    });
    rootNode.appendChild(node);
};

const rootNode = document.createElement('div');
document.body.appendChild(rootNode);

const render = (hash, term) => {
    if (!window.argsData || window.argsData.hash != hash) {
        window.argsData = { hash, args: null };
    }
    console.log('rendering', hash, term);
    window
        .loadUnison(`/build/${hash}/.${term}`, `/build/${hash}/.${term}/names`)
        .then((runtime) => {
            const [args, effects, result] = runtime.info(term);
            rootNode.innerHTML = '';
            console.log('Reloading!');
            console.log(args, effects, result);
            const handlers = window.makeDefaultHandlers(runtime, rootNode);
            // ok no effects needed
            if (!args.length) {
                showResult(runtime.runSync(term, [], {}));
            } else if (
                window.argsData.args &&
                window.argsData.args.every((m) => m !== undefined)
            ) {
                if (runtime.canRunSync(term, handlers)) {
                    showResult(
                        runtime.runSync(term, window.argsData.args, handlers),
                    );
                } else {
                    runtime.run(term, window.argsData.args, handlers);
                }
            } else {
                if (!window.argsData.args) {
                    window.argsData.args = args.map((_) => undefined);
                }
            }
        });
};

window.onhashchange = main;
