// ok
/** @jsx jsx */
import { jsx } from '@emotion/core';
import * as React from 'react';

const Term = ({ args, state, depth, path, type, canEval, setState }) => {
    const name = path.join('.');
    return (
        <div
            onClick={
                canEval
                    ? () =>
                          setState((state) => ({
                              ...state,
                              watchers: {
                                  ...state.watchers,
                                  [name]: state.watchers[name]
                                      ? null
                                      : {
                                            args: canEval[0],
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

const Branch = ({ state, ns, setState, depth }) => {
    const [isOpen, setOpen] = React.useState(false);

    const key = ns.length ? ns.join('.') : '';
    React.useEffect(() => {
        const head = state.head;
        if (isOpen && !state.tree[key]) {
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
                            tree: { ...state.tree, [key]: data },
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
                    {state.tree[key] ? (
                        <React.Fragment>
                            {state.tree[key][0].map((child) => (
                                <Branch
                                    depth={depth + 1}
                                    key={child}
                                    state={state}
                                    ns={ns.concat([child])}
                                    setState={setState}
                                />
                            ))}
                            {state.tree[key][1].map(([name, type, canEval]) => (
                                <Term
                                    depth={depth + 1}
                                    key={name}
                                    state={state}
                                    path={ns.concat([name])}
                                    type={type}
                                    setState={setState}
                                    canEval={canEval}
                                />
                            ))}
                        </React.Fragment>
                    ) : (
                        'Loading...'
                    )}
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
