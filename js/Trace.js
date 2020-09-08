// Ok

import * as React from 'react';

const Trace = ({ trace, names }) => {
    const [open, setOpen] = React.useState({ 0: true });
    const [text, setText] = React.useState('');

    return (
        <div>
            <input
                value={text}
                onChange={(evt) => setText(evt.target.value)}
                onKeyDown={(evt) => {
                    if (evt.key === 'Enter') {
                        setOpen((open) => ({ ...open, [text]: true }));
                    }
                }}
            />
            {Object.keys(trace)
                .sort((a, b) => +a - +b)
                .map((k) =>
                    open[k] ? (
                        <Item
                            tid={k}
                            key={k}
                            names={names}
                            data={trace[k]}
                            onClose={() =>
                                setOpen((open) => ({ ...open, [k]: false }))
                            }
                            onOpen={(ok) =>
                                setOpen((open) => ({ ...open, [ok]: true }))
                            }
                        />
                    ) : null,
                )}
        </div>
    );
};

const styles = {
    value: {
        fontFamily: 'monospace',
        whiteSpace: 'pre-wrap',
        backgroundColor: '#eee',
        padding: '2px 4px',
    },
};

const Value = ({ value, names }) => {
    const [isOpen, setOpen] = React.useState(false);
    const k = Object.keys(value)[0];
    if (['Int', 'Float', 'Nat', 'Boolean', 'Text', 'Char'].includes(k)) {
        return (
            <span style={styles.value}>
                {k}: {value[k]}
            </span>
        );
    }
    if (k === 'Ref' && value[k].DerivedId) {
        return (
            <span style={styles.value}>
                Ref <Hash hash={value[k].DerivedId[0]} names={names} />
            </span>
        );
    }
    if (k === 'RequestWithContinuation') {
        const [ref, _, args, __, frames, ___] = value[k];
        return (
            <span style={styles.value} onClick={() => setOpen(!isOpen)}>
                RequestWithContinuation:{' '}
                <Hash hash={ref.DerivedId[0]} names={names} />
                {isOpen ? (
                    <div>
                        <div>
                            {args.map((arg, i) => (
                                <Value key={i} value={arg} names={name} />
                            ))}
                        </div>
                        <div>
                            {frames.map((frame, i) => (
                                <Frame frame={frame} names={names} key={i} />
                            ))}
                        </div>
                    </div>
                ) : null}
            </span>
        );
    }
    if (k === 'PartialFnBody') {
        return (
            <span style={styles.value} onClick={() => setOpen(!isOpen)}>
                PartialFn({value[k][0]}) - {value[k][1].length} bindings
                {isOpen ? ': ' + JSON.stringify(value[k]) : null}
            </span>
        );
    }
    return (
        <span onClick={() => setOpen(!isOpen)} style={styles.value}>
            {k}
            {isOpen ? ': ' + JSON.stringify(value[k]) : null}
        </span>
    );
};

const key = (o) => (typeof o === 'string' ? o : Object.keys(o)[0]);

const IR = ({ ir, names }) => {
    const k = key(ir);
    if (k === 'Value') {
        return (
            <span>
                Value: <Value names={names} value={ir.Value} />
            </span>
        );
    }
    if (k === 'Fn') {
        return (
            <span style={styles.value}>
                Fn declaration ({ir[k][0]}) - {JSON.stringify(ir[k][1])}
            </span>
        );
    }
    if (k === 'PopAndName') {
        return (
            <span style={styles.value}>
                PopAndName {ir[k][0].text}({ir[k][0].unique})
            </span>
        );
    }
    if (typeof ir === 'string') {
        return (
            <span style={{ ...styles.value, backgroundColor: '#ccc' }}>
                {ir}
            </span>
        );
    }
    return <span style={styles.value}>{JSON.stringify(ir)}</span>;
};

const Event = ({ event, names, onOpen }) => {
    const k = key(event);
    if (k === 'Push') {
        return (
            <div>
                ➡️ push(
                <Value names={names} value={event[k]} />
                {/* <span style={styles.value}>{JSON.stringify(event.value)}</span> */}
                )
            </div>
        );
    }
    if (k === 'Pop') {
        return (
            <div>
                ⬅️ pop(
                <Value names={names} value={event[k]} />
                {/* <span style={styles.value}>{JSON.stringify(event.value)}</span> */}
                )
            </div>
        );
    }
    if (k === 'IR') {
        const [idx, cmd] = event[k];
        return (
            <div>
                🏃‍♀️[{idx}] <IR names={names} ir={cmd} />
                <span style={{ fontSize: '80%' }}>
                    {' ' + (event.end - event.start)}ms
                </span>
            </div>
        );
    }
    if (k === 'Ret') {
        return (
            <div>
                🏁 <Ret ret={event[k]} names={names} />
            </div>
        );
    }
    if (k === 'JumpBack') {
        return (
            <button onClick={() => onOpen(event[k])}>⭐↩️ : {event[k]}</button>
        );
    }
    if (k === 'CloneFrame') {
        return (
            <button onClick={() => onOpen(event[k])}>⭐⏯ : {event[k]}</button>
        );
    }
    if (k === 'NewFrame') {
        return (
            <button onClick={() => onOpen(event[k])}>⭐ : {event[k]}</button>
        );
    }
    return <div>{JSON.stringify(event)}</div>;
};

const Frame = ({ frame, names }) => {
    return (
        <div style={styles.value}>
            [{frame.trace_id}] - <Source names={names} source={frame.source} />{' '}
            Handler: {frame.handler} Return Idx: {frame.return_index}
            <Reveal
                short={` Stack: ${frame.stack.length}`}
                full={JSON.stringify(frame.stack)}
            />
            <Reveal
                short={` Bindings: ${frame.bindings.length}`}
                full={JSON.stringify(frame.bindings)}
            />
        </div>
    );
};

const Reveal = ({ short, full }) => {
    const [open, setOpen] = React.useState(false);
    return (
        <span onClick={() => setOpen(!open)}>
            {short} {open ? full : null}
        </span>
    );
};

const Ret = ({ ret, names }) => {
    const k = key(ret);
    if (k === 'Value') {
        return (
            <span>
                Value: <Hash hash={ret[k]} names={names} />
            </span>
        );
    }
    if (k === 'Request') {
        return (
            <span>
                Request: <Value value={ret[k][0]} names={names} />
            </span>
        );
    }
    if (k === 'ReRequest') {
        return (
            <span>
                ReRequest: <Value value={ret[k][0]} names={names} />
                <div>
                    {ret[k][4].map((frame, i) => (
                        <Frame names={names} frame={frame} key={i} />
                    ))}
                </div>
            </span>
        );
    }
    if (k === 'FnCall') {
        return (
            <span>
                Fn({ret[k][0]}) - {ret[k][1].length} bindings -{' '}
                <Value value={ret[k][2]} names={names} />
            </span>
        );
    }
    return <span style={styles.value}>{JSON.stringify(ret)}</span>;
};

const Hash = ({ hash, names }) => {
    if (!names) {
        return 'NO NAMES';
    }
    if (names[0][hash]) {
        names[0][hash].sort((a, b) => a.length - b.length);
        return (
            <span style={styles.value}>{`${names[0][hash][0].join(
                '.',
            )} : #${hash.slice(0, 10)}`}</span>
        );
    } else {
        return <span style={styles.value}>{`#${hash.slice(0, 10)}`}</span>;
    }
};

const Source = ({ source, names }) => {
    // js style
    // if (source.type === 'term') {
    //     return (
    //         <span>
    //             Term(
    //             <Hash hash={source.hash} names={names} />)
    //         </span>
    //     );
    // }
    // if (source.type === 'Fn') {
    //     return (
    //         <span>
    //             Fn({source.fnid} - <Hash hash={source.hash} names={names} />)
    //         </span>
    //     );
    // }
    // rust style
    if (source.Value) {
        return (
            <span>
                Term(
                <Hash hash={source.Value} names={names} />)
            </span>
        );
    }
    const [fnid, hash] = source.Fn;
    return (
        <span>
            Fn({fnid} - <Hash hash={hash} names={names} />)
        </span>
    );
};

const Item = ({
    tid,
    data: { start, end, events, source, frame },
    names,
    onOpen,
    onClose,
}) => {
    return (
        <div style={{ padding: 12 }}>
            <div>
                <span
                    style={{
                        backgroundColor: '#cfc',
                        border: '1px solid #55c',
                        padding: '2px 4px',
                        display: 'inline-block',
                        margin: 4,
                    }}
                >
                    {tid}
                </span>
                <Source source={frame} names={names} /> :{' '}
                {JSON.stringify(source)}
                <button onClick={() => onClose()}>❌</button>
            </div>
            <div>
                Duration: {end != null ? end - start + 'ms' : 'Incomplete'}
            </div>
            <div>
                {events.map((event, i) => (
                    <div style={{ display: 'flex' }}>
                        [{i}]
                        <Event
                            onOpen={onOpen}
                            key={i}
                            names={names}
                            event={event}
                        />
                    </div>
                    // <div key={i}>{JSON.stringify(event)}</div>
                ))}
            </div>
        </div>
    );
};

export default Trace;
