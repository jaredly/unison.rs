// What I imagine.

const cmp = (a, b, c) => {
    if (a == b) {
        return c == null ? 0 : c;
    }
    return a < b ? -1 : 1;
};

const showTerms = (head, ns) => {
    getTerms(head, ns).then(([children, terms]) => {
        navNode.innerHTML = '';
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
            navNode.appendChild(button);
        });
        terms
            .sort((a, b) => cmp(b[2], a[2], cmp(a[0], b[0])))
            .forEach(([term, type, can_exec]) => {
                const node = document.createElement(
                    can_exec ? 'button' : 'div',
                );
                node.textContent = term + ' : ' + type;
                Object.assign(node.style, {
                    display: 'block',
                    fontFamily: 'monospace',
                    textAlign: 'left',
                    background: 'none',
                    cursor: can_exec ? 'pointer' : 'normal',
                    border: 'none',
                    color: can_exec ? 'blue' : 'black',
                    padding: '0',
                    margin: '8px 0',
                });
                if (can_exec) {
                    node.onclick = () => {
                        window.location.hash =
                            '#render:' + ns.concat(term).join('.');
                    };
                }
                navNode.appendChild(node);
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
const navNode = document.createElement('div');
document.body.appendChild(navNode);
document.body.appendChild(rootNode);

const render = (hash, term) => {
    navNode.innerHTML = '';
    if (!window.argsData || window.argsData.term != term) {
        console.log('RESET HASH');
        window.argsData = { term, args: null };
    }
    console.log('Args data', window.argsData, hash);
    console.log('rendering', hash, term);
    window
        .loadUnison(`/build/${hash}/.${term}`, `/build/${hash}/.${term}/names`)
        .then((runtime) => {
            const args = runtime.info(term);
            rootNode.innerHTML = '';
            console.log('Reloading!', args);
            const handlers = window.makeDefaultHandlers(runtime, rootNode);
            if (!window.argsData.args) {
                window.argsData.args = args.map((arg) =>
                    arg == null ? null : undefined,
                );
            }
            // ok no effects needed
            if (!args.length) {
                showResult(runtime.runSync(term, [], {}));
            } else if (window.argsData.args.every((m) => m !== undefined)) {
                if (runtime.canRunSync(term, handlers)) {
                    showResult(
                        runtime.runSync(term, window.argsData.args, handlers),
                    );
                } else {
                    runtime.run(term, window.argsData.args, handlers);
                }
            } else {
                getArgs(runtime, term, args, handlers);
                // console.log('Need some args', args);
            }
        });
};

const getArgs = (runtime, term, args, handlers) => {
    args.forEach((arg, i) => {
        if (arg == null) {
            return; // don't need it
        }
        if (arg == 'Nat') {
            const node = document.createElement('input');
            node.type = 'number';
            node.onchange = (evt) =>
                (window.argsData.args[i] = parseInt(evt.target.value) | 0);
            rootNode.appendChild(node);
        }
    });
    const button = document.createElement('button');
    button.textContent = 'run';
    button.onclick = () => {
        runtime.run(term, window.argsData.args, handlers);
    };
    rootNode.appendChild(button);
};

const main = () => {
    if (!latestHash) {
        return;
    }
    const hash = window.location.hash.slice(1);
    if (hash.startsWith('render:')) {
        render(latestHash, hash.slice('render:'.length));
    } else {
        rootNode.innerHTML = '';
        showTerms(latestHash, hash ? hash.split('.') : []);
    }
};

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

window.onhashchange = main;
