let x = {
    PartialConstructor: [
        {
            DerivedId: [
                'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0',
                0,
                1,
            ],
        },
        0,
        [
            {
                PartialConstructor: [
                    {
                        DerivedId: [
                            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo',
                            0,
                            1,
                        ],
                    },
                    0,
                    [{ Nat: 1 }],
                ],
            },
            {
                PartialConstructor: [
                    {
                        DerivedId: [
                            'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0',
                            0,
                            1,
                        ],
                    },
                    0,
                    [
                        {
                            PartialConstructor: [
                                {
                                    DerivedId: [
                                        'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo',
                                        0,
                                        1,
                                    ],
                                },
                                0,
                                [{ Nat: 0 }],
                            ],
                        },
                        {
                            Constructor: [
                                {
                                    DerivedId: [
                                        '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8',
                                        0,
                                        1,
                                    ],
                                },
                                0,
                            ],
                        },
                    ],
                ],
            },
        ],
    ],
};

const key = (x) => (typeof x === 'string' ? x : Object.keys(x)[0]);

const lookup_name = ([_, constructors, __], hash, n) => {
    if (constructors[hash]) {
        if (constructors[hash][n]) {
            const names = constructors[hash][n];
            names.sort((a, b) => a.length - b.length);
            return names[0].join('.');
        }
    }
};

const tuple_hash =
    'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0';

const unit_hash =
    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8';

const flatten_tuple = (value) => {
    if (
        value.PartialConstructor &&
        value.PartialConstructor[0].DerivedId &&
        value.PartialConstructor[0].DerivedId[0] === tuple_hash
    ) {
        const [left, right] = value.PartialConstructor[2];
        const rr = flatten_tuple(right);
        if (rr != null) {
            return [left].concat(rr);
        } else if (
            right.Constructor &&
            right.Constructor[0].DerivedId &&
            right.Constructor[0].DerivedId[0] === unit_hash
        ) {
            return [left];
        } else {
            return [left, right];
        }
    }
};

export const pretty_print = (names, value) => {
    const flat = flatten_tuple(value);
    if (flat != null) {
        return { Tuple: flat.map((v) => pretty_print(names, v)) };
    }
    const t = key(value);
    if (t === 'Nat' || t === 'Int' || t === 'Float') {
        return value[t];
    } else if (t === 'PartialConstructor') {
        const [ref, n, values] = value[t];
        const rt = key(ref);
        if (rt === 'DerivedId') {
            const [hash, _, __] = ref[rt];
            return {
                [lookup_name(names, hash, n)]:
                    values.length === 1
                        ? pretty_print(names, values[0])
                        : values.map((v) => pretty_print(names, v)),
            };
        } else if (rt === 'Builtin') {
            // if (ref[rt] === 'Sequence') {
            //     return values.map((v) => pretty_print(names, v));
            // } else if (
            //     ref[rt] === 'Nat' ||
            //     ref[rt] === 'Int' ||
            //     ref[rt] === 'Float'
            // ) {
            //     return values[0];
            // } else {
            return { [ref[rt]]: values.map((v) => pretty_print(names, v)) };
            // }
        }
    }
    return value;
};
