// ok
/** @jsx jsx */
import { jsx } from '@emotion/core';
import * as React from 'react';

import { load as loadRuntime } from './unison';
import Sidebar from './Sidebar';
import Watchers from './Watchers';

/*
Ok, general plan:
- at the top level, we have a listener that reloads stuff when the head changes, right?
- I mean
not a full reload.

sidebar gets data from one place
ummmm

So we have "watchers", right?
which is toplevel state, probably should persist across refresh?
yeah let's localstorage that.

And then everything else can refresh? probably.

sidebar collapse state might be another thing ...

*/

const initialState = {
    watchers: {},
    head: null,
    runtime: null,
    tree: {},
};

const App = () => {
    const [state, setState] = React.useState(initialState);

    React.useEffect(() => {
        const ws = new WebSocket(`ws://${location.host}/reload-notifier`);

        ws.addEventListener('message', (evt) => {
            setState((state) => ({ ...state, head: evt.data }));
        });
    }, []);

    React.useEffect(() => {
        if (!state.head) {
            return;
        }
        const head = state.head;

        const watchers = Object.keys(state.watchers);
        if (!watchers.length) {
            return;
        }
        const dataUrl = fetch(
            `/build/${head}/${watchers.join(',')}`,
        ).then((r) => r.text());
        const namesUrl = fetch(
            `/build/${head}/${watchers.join(',')}/names`,
        ).then((r) => r.json());
        loadRuntime(dataUrl, namesUrl).then((runtime) =>
            setState((state) => {
                if (state.head !== head) {
                    return state;
                }
                return { ...state, runtime };
            }),
        );
    }, [state.head]);

    return (
        <div
            css={{
                display: 'flex',
                width: '100vw',
                height: '100vh',
            }}
        >
            <Sidebar state={state} setState={setState} />
            <div>
                {state.head}
                <Watchers
                    tree={state.tree}
                    runtime={state.runtime}
                    watchers={state.wathcers}
                    setWatchers={(watchers) =>
                        setState((state) => ({ ...state, watchers }))
                    }
                />
            </div>
        </div>
    );
};

export default App;
