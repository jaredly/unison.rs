const key = (o) => (typeof o === 'string' ? o : Object.keys(o)[0]);

const compare = (a, b) => {
    const ka = key(a);
    const kb = key(b);

    if (ka !== kb) {
        return ka < kb ? -1 : 1;
    }

    if (['Int', 'Nat', 'Float', 'Boolean'].includes(ka)) {
        return a[ka] - b[kb];
    }

    if (['Text', 'Char'].includes(ka)) {
        return a[ka] < b[kb] ? -1 : a[ka] > b[kb] ? 1 : 0;
    }

    if (ka === 'Bytes') {
        return a.join('') === b.join('');
    }

    const sa = JSON.stringify(a);
    const sb = JSON.stringify(b);
    // console.log('Compare');
    // console.log(sa);
    // console.log(sb);
    // console.log(sa == sb);
    // STOPSHIP this will fall apart with symbols, because they need to be compared by unique
    return sa < sb ? -1 : sa > sb ? 1 : 0;
};

export default compare;
