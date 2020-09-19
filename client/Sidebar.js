// ok
/** @jsx jsx */
import { jsx } from '@emotion/core';
import * as React from 'react';

const rm = (obj, k) => {
    delete obj[k];
    return obj;
};

const Term = ({ args, state, depth, path, type, canEval, setState }) => {
    const name = path.join('.');
    return (
        <div
            onClick={
                canEval
                    ? () =>
                          setState((state) => ({
                              ...state,
                              runtime: null,
                              watchers: state.watchers[name]
                                  ? rm({ ...state.watchers }, name)
                                  : {
                                        ...state.watchers,
                                        [name]: {
                                            path,
                                            args: canEval[0],
                                            values: canEval[0].map(
                                                () => undefined,
                                            ),
                                        },
                                    },
                          }))
                    : null
            }
            css={{
                cursor: canEval ? 'pointer' : null,
                padding: '8px 16px',
                paddingLeft: 16 * (depth + 1),
                fontFamily: 'monospace',
                color: canEval ? 'blue' : 'black',
                ':hover': canEval
                    ? {
                          backgroundColor: '#efe',
                      }
                    : null,
            }}
        >
            {name}
            {state.watchers[name] ? ' [watch]' : null}
        </div>
    );
};

const Type = ({ path, depth }) => {
    return (
        <div>
            <div
                css={{
                    padding: '8px 16px',
                    paddingLeft: 16 * (depth + 1),
                    fontFamily: 'monospace',
                }}
                onClick={() => setOpen(!isOpen)}
            >
                {path.join('.')}
            </div>
        </div>
    );
};

const styles = {
    heading: {
        margin: 0,
        padding: '8px 16px',
        fontSize: '80%',
    },
};

const Branch = ({ state, ns, setState, depth }) => {
    const key = ns.length ? ns.join('.') : '';
    const isOpen = state.expanded[key];
    const setOpen = (open) =>
        setState((state) => ({
            ...state,
            expanded: { ...state.expanded, [key]: open },
        }));

    React.useEffect(() => {
        const head = state.head;
        if (
            isOpen &&
            head &&
            (!state.tree[key] || state.tree[key].head !== head)
        ) {
            fetch(`/terms/${state.head}/${key}`)
                .then((r) => r.json())
                .then((data) => {
                    console.log('got it');
                    setState((state) => {
                        if (state.head !== head) {
                            console.log('diff head, sorry');
                            return state;
                        }
                        console.log('yes please', key);
                        return {
                            ...state,
                            tree: { ...state.tree, [key]: { head, data } },
                        };
                    });
                });
        }
    }, [isOpen, state.head]);
    return (
        <div>
            <div
                css={{
                    cursor: 'pointer',
                    padding: '8px 16px',
                    paddingLeft: 16 * (depth + 1),
                    fontSize: '80%',
                    fontWeight: 500,
                    ':hover': {
                        backgroundColor: '#efe',
                    },
                }}
                onClick={() => setOpen(!isOpen)}
            >
                {ns.join('.') + '/'}
            </div>
            {isOpen ? (
                <div
                    css={
                        {
                            // paddingLeft: 16,
                        }
                    }
                >
                    {
                        state.tree[key] ? (
                            <React.Fragment>
                                {state.tree[key].data[0].length ? (
                                    <h4
                                        css={[
                                            styles.heading,
                                            {
                                                paddingLeft: 16 * (depth + 2),
                                            },
                                        ]}
                                    >
                                        Namespaces
                                    </h4>
                                ) : null}
                                {state.tree[key].data[0]
                                    .filter((child) => child.trim())
                                    .sort()
                                    .map((child) => (
                                        <Branch
                                            depth={depth + 1}
                                            key={child}
                                            state={state}
                                            ns={ns.concat([child])}
                                            setState={setState}
                                        />
                                    ))}
                                {state.tree[key].data[1].length ? (
                                    <h4
                                        css={[
                                            styles.heading,
                                            {
                                                paddingLeft: 16 * (depth + 2),
                                            },
                                        ]}
                                    >
                                        Terms
                                    </h4>
                                ) : null}
                                {state.tree[key].data[1].map(
                                    ([name, type, canEval, hash]) => (
                                        <Term
                                            depth={depth + 1}
                                            key={name}
                                            state={state}
                                            path={ns.concat([name])}
                                            type={type}
                                            setState={setState}
                                            canEval={canEval}
                                        />
                                    ),
                                )}
                                {state.tree[key].data[2].length ? (
                                    <h4
                                        css={[
                                            styles.heading,
                                            {
                                                paddingLeft: 16 * (depth + 2),
                                            },
                                        ]}
                                    >
                                        Types
                                    </h4>
                                ) : null}
                                {state.tree[key].data[2].sort().map((child) => (
                                    <Type
                                        key={child}
                                        depth={depth + 1}
                                        path={ns.concat([child])}
                                    />
                                ))}
                                {state.tree[key].data[3].length ? (
                                    <h4
                                        css={[
                                            styles.heading,
                                            {
                                                paddingLeft: 16 * (depth + 2),
                                            },
                                        ]}
                                    >
                                        Constructors
                                    </h4>
                                ) : null}
                                {state.tree[key].data[3]
                                    .sort()
                                    .map(([name, hash, idx]) => (
                                        <Type
                                            key={name}
                                            depth={depth + 1}
                                            path={ns.concat([name])}
                                        />
                                    ))}
                            </React.Fragment>
                        ) : null
                        // 'Loading...'
                    }
                </div>
            ) : null}
        </div>
    );
};

const Sidebar = ({ state, setState }) => {
    return (
        <div
            css={{
                width: 400,
                flexShrink: 0,
                overflow: 'auto',
                backgroundColor: '#dcf2fd',
            }}
        >
            <Branch depth={0} state={state} setState={setState} ns={[]} />
        </div>
    );
};

export default Sidebar;
