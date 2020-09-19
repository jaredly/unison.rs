// ok
/** @jsx jsx */
import { jsx } from '@emotion/core';
import * as React from 'react';
import makeHandlers from './handlers';

const rm = (obj, k) => {
    delete obj[k];
    return obj;
};

const Watch = ({ head, name, hash, runtime, config, unWatch }) => {
    const [result, setResult] = React.useState(null);

    const canRun =
        config.args.length == 0 ||
        config.args.every(
            (v, i) => v == null || config.values[i] !== undefined,
        );
    const output = React.useRef(null);

    const handlers = React.useRef(null);
    React.useEffect(() => {
        if (output.current && runtime) {
            handlers.current = makeHandlers(runtime, output.current);
        }
    }, [output.current, !!runtime, head]);

    const hasRun = React.useRef(false);
    const oldHash = React.useRef(hash);
    if (oldHash.current !== hash) {
        oldHash.current = hash;
        hasRun.current = false;
    }

    React.useEffect(() => {
        if (!canRun || !handlers.current || !runtime) {
            console.log('not ready', config);
            return;
        }
        // TODO: when updating config values, need to clear out hasRun.
        // maybe change the name to "needsRun"
        if (hasRun.current) {
            console.log('skip run');
            return;
        }
        console.log('running with', runtime.head);
        hasRun.current = true;
        if (
            config.args.length === 0 ||
            runtime.canRunSync(name, handlers.current)
        ) {
            setResult(runtime.runSync(name, config.values, handlers.current));
        } else {
            runtime.run(name, config.values, handlers.current);
        }
    }, [!!runtime, hash, canRun, handlers.current, config]);
    return (
        <div
            css={{
                padding: 16,
                margin: 8,
                boxShadow: '0 0 3px #ccc',
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'stretch',
            }}
        >
            <div css={{ display: 'flex', justifyContent: 'space-between' }}>
                <div
                    css={{
                        fontWeight: 500,
                        marginRight: 16,
                    }}
                >
                    {name}
                </div>
                {canRun && result != null ? (
                    <div css={{ whiteSpace: 'pre', fontFamily: 'monospace' }}>
                        {JSON.stringify(result)}
                    </div>
                ) : null}
            </div>
            <div key={hash} ref={output} />
        </div>
    );
};

const Watchers = ({ state, setState }) => {
    return (
        <div css={{ flex: 1, overflow: 'auto' }}>
            <h3 css={{ margin: 0, padding: 32 }}>Watchers</h3>
            {Object.keys(state.watchers)
                .filter((k) => state.watchers[k])
                .map((key) => {
                    return (
                        <Watch
                            key={key}
                            name={key}
                            head={state.head}
                            runtime={state.runtime}
                            config={state.watchers[key]}
                            hash={findHash(
                                state.tree,
                                state.watchers[key].path,
                            )}
                            unWatch={() =>
                                setState((state) => ({
                                    ...state,
                                    watchers: rm(
                                        {
                                            ...state.watchers,
                                        },
                                        key,
                                    ),
                                }))
                            }
                        />
                    );
                    // const config = state.watchers[watcher]
                })}
        </div>
    );
};

const findHash = (tree, path) => {
    const parent = path.slice(0, -1).join('.');
    const last = path[path.length - 1];
    if (!tree[parent]) {
        console.log('no hash', tree, parent, path);
        return null;
    }
    for (let child of tree[parent].data[1]) {
        if (child[0] == last) {
            return child[3];
        }
    }
    console.log('no child', last, tree[parent].data[1]);
    return null;
};

export default Watchers;
