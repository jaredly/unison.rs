// Ok

import * as React from 'react';

const Trace = ({ trace, names }) => {
    const [open, setOpen] = React.useState({ 0: true });

    return (
        <div>
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
    if (k === 'Ref') {
        return (
            <span style={styles.value}>
                Ref <Hash hash={value[k].DerivedId[0]} names={names} />
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
        return <Value names={names} value={ir.Value} />;
    }
    if (k === 'Fn') {
        return <span style={styles.value}>Fn declaration ({ir[k][0]})</span>;
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
    if (event.type === 'push') {
        return (
            <div>
                ➡️ push(
                <Value names={names} value={event.value} />
                {/* <span style={styles.value}>{JSON.stringify(event.value)}</span> */}
                )
            </div>
        );
    }
    if (event.type === 'pop') {
        return (
            <div>
                ⬅️ pop(
                <Value names={names} value={event.value} />
                {/* <span style={styles.value}>{JSON.stringify(event.value)}</span> */}
                )
            </div>
        );
    }
    if (event.type === 'ir') {
        return (
            <div>
                🏃‍♀️[{event.idx}] <IR names={names} ir={event.cmd} />
            </div>
        );
    }
    if (event.type === 'ret') {
        return (
            <div>
                🏁 <Ret ret={event.ret} names={names} />
            </div>
        );
    }
    if (event.type === 'new_frame') {
        return (
            <button onClick={() => onOpen(event.traceId)}>
                ⭐ : {event.traceId}
            </button>
        );
    }
    return <div>{JSON.stringify(event)}</div>;
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
    if (source.type === 'term') {
        return (
            <span>
                Term(
                <Hash hash={source.hash} names={names} />)
            </span>
        );
    }
    return (
        <span>
            Fn({source.fnid} - <Hash hash={source.hash} names={names} />)
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
                    <Event
                        onOpen={onOpen}
                        key={i}
                        names={names}
                        event={event}
                    />
                    // <div key={i}>{JSON.stringify(event)}</div>
                ))}
            </div>
        </div>
    );
};

export default Trace;
