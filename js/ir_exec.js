// ir_exec

import compare from './compare';
import { patternMatch } from './pattern';

const option_hash =
    '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8';
const option_ref = { DerivedId: [option_hash, 0, 1] };

// pub enum Ret {
//     FnCall(usize, Vec<(Symbol, usize, Arc<Value>)>, Arc<Value>),
//     Value(Hash),
//     Nothing,
//     Request(Reference, usize, Vec<Arc<Value>>),
//     ReRequest(Reference, usize, Vec<Arc<Value>>, usize, Vec<Frame>, usize),
//     Handle(usize),
//     HandlePure,
//     Continue(usize, Vec<Frame>, Arc<Value>),
// }
const key = (o) => (typeof o === 'string' ? o : Object.keys(o)[0]);

const handlers = {
    Swap: (_, state) => {
        let one = state.stack.pop();
        let two = state.stack.pop();
        state.stack.push(one);
        state.stack.push(two);
        state.idx += 1;
    },
    Handle: (mark_idx, state) => ({ Handle: mark_idx }),
    HandlePure: (_, state) => {
        let v = state.stack.pop();
        if (!v.RequestPure) {
            v = { RequestPure: v };
        }
        state.stack.push(v);
        return 'HandlePure';
    },
    Value: (term, state) => {
        if (term.Request) {
            const [a, b] = term.Request;
            state.idx += 1;
            return { Request: [a, b, []] };
        }
        if (term.RequestWithArgs) {
            const [a, b, n, args] = term.RequestWithArgs;
            if (n === args.length) {
                state.idx += 1;
                return { Request: [a, b, args] };
            }
        }
        if (term.Ref && term.Ref.DerivedId) {
            const [hash, _, __] = term.Ref.DerivedId;
            state.idx += 1;
            return { Value: hash };
        }
        state.stack.push(term);
        state.idx += 1;
    },
    PushSym: ([symbol, usage], state) => {
        const v = state.stack.get_vbl(symbol, usage);
        state.stack.push(v);
        state.idx += 1;
    },
    Pop: (_, state) => {
        state.stack.pop();
        state.idx += 1;
    },
    PopAndName: ([symbol, uses], state) => {
        const v = state.stack.pop();
        state.stack.bind([symbol, uses, v]);
        state.idx += 1;
    },
    Fn: ([i, free_vbls], state) => {
        const bound = free_vbls.map(([sym, external, internal, cycle], i) => {
            return [
                { ...sym, unique: i },
                internal,
                cycle
                    ? { CycleBlank: sym.unique }
                    : state.stack.get_vbl(sym, external),
            ];
        });
        state.stack.push({ PartialFnBody: [i, bound] });
        state.idx += 1;
    },
    Cycle: (names, state) => {
        const mutuals = [];
        const items = [];

        for (let [name, uses] of names) {
            let v = state.stack.pop();
            if (v.PartialFnBody) {
                const [fnint, bindings] = v.PartialFnBody;
                mutuals.push([name, uses, fnint, bindings]);
                items.push([name, uses, fnint, bindings]);
            } else {
                state.stack.bindLast([name, uses, v]);
            }
        }
        for (let [name, uses, fnint, bindings] of items) {
            state.stack.bindLast([
                name,
                uses,
                { CycleFnBody: [fnint, bindings, mutuals] },
            ]);
        }
        state.idx += 1;
    },
    Call: (_, state) => {
        // console.log('Call!');
        let arg = state.stack.pop();
        let f = state.stack.pop();
        // console.log(f, arg);
        const typ = key(f);
        if (!call[typ]) {
            console.log('Bad call', typ, f, arg);
        }
        return call[typ](f[typ], arg, state);
    },
    Seq: (num, state) => {
        const v = [];
        for (let i = 0; i < num; i++) {
            // TODO would be nice to ditch the wrappings
            v.unshift(state.stack.pop());
        }
        state.stack.push({ Sequence: v });
        state.idx += 1;
    },
    JumpTo: (mark, state) => {
        state.idx = mark;
    },
    Mark: (_mark, state) => {
        // already collected as marks
        state.idx += 1;
    },
    If: (mark, state) => {
        const v = state.stack.pop();
        if (v.Boolean === true) {
            state.idx += 1;
        } else if (v.Boolean === false) {
            state.idx = mark;
        } else {
            throw new Error(`If not on a bool ${v}`);
        }
    },
    IfAndPopStack: (mark, state) => {
        const v = state.stack.pop();
        if (v.Boolean === true) {
            state.idx += 1;
        } else if (v.Boolean === false) {
            state.idx = mark;
            state.stack.pop_to_mark();
        } else {
            throw new Error(`If not on a bool ${v}`);
        }
    },
    MarkStack: (_, state) => {
        state.stack.mark();
        state.idx += 1;
    },
    ClearStackMark: (_, state) => {
        state.stack.clear_mark();
        state.idx += 1;
    },
    PatternMatch: ([pattern, has_where], state) => {
        const value = state.stack.peek();
        // console.log(`MATCH`);
        // console.log(pattern);
        // console.log(value);
        const bindings = patternMatch(pattern, value);
        // console.log('RESULT', bindings);
        if (!bindings) {
            state.stack.push({ Boolean: false });
        } else {
            bindings.reverse();
            if (has_where) {
                for (let term of bindings) {
                    state.stack.push(term);
                }
            }
            for (let term of bindings) {
                state.stack.push(term);
            }
            state.stack.push({ Boolean: true });
        }
        state.idx += 1;
    },
    PatternMatchFail: (_, state) => {
        const value = state.stack.pop();
        if (value.RequestWithContinuation) {
            const [
                req,
                i,
                args,
                back_idx,
                frames,
                current_idx,
            ] = value.RequestWithContinuation;
            return {
                ReRequest: [req, i, args, back_idx, frames, current_idx],
            };
        } else {
            console.log(state.pretty_print(value));
            throw new Error(`Pattern match fail ${JSON.stringify(value)}`);
        }
    },
    PopUpOne: (_, state) => {
        state.stack.pop_up();
        state.idx += 1;
    },
};

export const eval_ir = (cmd, state) => {
    const tag = key(cmd);
    if (!handlers[tag]) {
        throw new Error(`no handler for ${tag}`);
    }
    return handlers[tag](cmd[tag], state);
};

const call = {
    Continuation([kidx, frames], arg, state) {
        state.idx += 1;
        return { Continue: [kidx, frames, arg] };
    },
    RequestWithArgs([r, i, n, args], arg, state) {
        state.idx += 1;
        args.push(arg);
        if (args.length == n) {
            return { Request: [r, i, args] };
        }
        state.stack.push({ RequestWithArgs: [r, i, n, args] });
    },
    Constructor([r, u], arg, state) {
        state.stack.push({ PartialConstructor: [r, u, [arg]] });
        state.idx += 1;
    },
    PartialConstructor([r, u, c], arg, state) {
        c.push(arg);
        state.stack.push({ PartialConstructor: [r, u, c] });
        state.idx += 1;
    },
    CycleFnBody([fnint, bindings, mutuals], arg, state) {
        state.idx += 1;
        bindings = bindings.map((b) => [...b]);
        for (let binding of bindings) {
            if (binding[2].CycleBlank) {
                const u = binding[2].CycleBlank;
                const [k, uses, fnid, sub_bindings] = mutuals.find(
                    (m) => m[0].unique === u,
                );
                binding[1] = uses;
                binding[2] = { CycleFnBody: [fnid, sub_bindings, mutuals] };
            }
        }

        return { FnCall: [fnint, bindings, arg] };
    },
    PartialFnBody([fnint, bindings], arg, state) {
        state.idx += 1;
        return { FnCall: [fnint, bindings, arg] };
    },
    Ref(inner, arg, state) {
        if (inner.Builtin) {
            return callSingleArgBuiltin(inner.Builtin, arg, state);
        } else {
            throw new Error('not yet');
        }
    },
    PartialNativeApp([name, args], arg, state) {
        return callMultiArgBuiltin(name, args, arg, state);
    },
};

const expectList = (v) => {
    if ('Sequence' in v) {
        return v.Sequence;
    }
    throw new Error('Not a list: ' + JSON.stringify(v));
};

const expectInt = (v) => {
    if ('Int' in v) {
        return v.Int;
    }
    throw new Error('Not an int: ' + JSON.stringify(v));
};

const expectNat = (v) => {
    if ('Nat' in v) {
        return v.Nat;
    }
    throw new Error('Not a nat: ' + JSON.stringify(v));
};

const expectFloat = (v) => {
    if ('Float' in v) {
        return v.Float;
    }
    throw new Error('Not a float: ' + JSON.stringify(v));
};

const singleArgBuiltins = {
    'Int.increment': (v) => ({ Int: expectInt(v) + 1 }),
    'Int.negate': (i) => ({ Int: -expectInt(i) }),
    'Int.isEven': (i) => ({ Boolean: expectInt(i) % 2 == 0 }),
    'Int.isOdd': (i) => ({ Boolean: expectInt(i) % 2 == 1 }),
    'Int.toText': (i) => ({ Text: expectInt(i).toString() }),
    'Int.complement': (i) => ({ Int: !expectInt(i) }), // STOPSHIP bit complement
    'Nat.increment': (i) => ({ Nat: expectNat(i) + 1 }),
    'Nat.isEven': (i) => ({ Boolean: expectNat(i) % 2 == 0 }),
    'Nat.isOdd': (i) => ({ Boolean: expectNat(i) % 2 == 1 }),
    'Nat.toInt': (i) => ({ Int: expectNat(i) }),
    'Nat.toText': (i) => ({ Text: expectNat(i).toString() }),
    'Nat.complement': (i) => ({ Nat: !expectNat(i) }),
    'Boolean.not': (i) => ({ Boolean: !expectBool(i) }),
    'List.size': (s) => ({ Nat: expectList(s).length }),
    'Text.size': (t) => ({ Nat: expectText(t).length }),
    'Text.toCharList': (t) => ({
        Sequence: expectText(t.split('').map((c) => ({ Char: c }))),
    }),
    'Text.fromCharList': (l) => ({ Text: l.map((c) => c.Char).join('') }),
    'Bytes.size': (t) => ({ Nat: expectBytes(t).length }),
    'Bytes.toList': (t) => ({
        Sequence: expectBytes(t).map((t) => ({ Nat: t })),
    }),
};

const callSingleArgBuiltin = (builtin, arg, state) => {
    if (singleArgBuiltins[builtin]) {
        state.stack.push(singleArgBuiltins[builtin](arg));
    } else {
        state.stack.push({ PartialNativeApp: [builtin, [arg]] });
    }
    state.idx += 1;
};

const callMultiArgBuiltin = (builtin, args, arg, state) => {
    if (multiArgBuiltins[builtin]) {
        state.stack.push(multiArgBuiltins[builtin](...args.concat([arg])));
        state.idx += 1;
    } else {
        throw new Error(`Unexpected builtin ${builtin}`);
    }
};

const multiArgBuiltins = {
    'Int.+': (a, b) => ({
        Int: wrapping_add(expectInt(a), expectInt(b)),
    }),
    'Int.-': (a, b) => ({
        Int: wrapping_sub(expectInt(a), expectInt(b)),
    }),
    'Int.*': (a, b) => ({ Int: expectInt(a) * expectInt(b) }),
    'Int./': (a, b) => ({ Int: (expectInt(a) / expectInt(b)) | 0 }),
    'Int.<': (a, b) => ({ Boolean: expectInt(a) < expectInt(b) }),
    'Int.<=': (a, b) => ({ Boolean: expectInt(a) <= expectInt(b) }),
    'Int.>': (a, b) => ({ Boolean: expectInt(a) > expectInt(b) }),
    'Int.>=': (a, b) => ({ Boolean: expectInt(a) >= expectInt(b) }),
    'Int.==': (a, b) => ({ Boolean: expectInt(a) == expectInt(b) }),
    'Int.and': (a, b) => ({ Int: expectInt(a) & expectInt(b) }),
    'Int.or': (a, b) => ({ Int: expectInt(a) | expectInt(b) }),
    'Int.xor': (a, b) => ({ Int: expectInt(a) ^ expectInt(b) }),
    'Int.mod': (a, b) => ({ Int: expectInt(a) % expectInt(b) }),
    'Int.pow': (a, b) => ({
        Int: Math.pow(expectInt(a), expectNat(b)),
    }),
    'Int.shiftLeft': (a, b) => ({
        Int: expectInt(a) << expectNat(b),
    }),
    'Int.shiftRight': (a, b) => ({
        Int: expectInt(a) >> expectNat(b),
    }),

    'Nat.+': (a, b) => ({
        Nat: expectNat(a) + expectNat(b),
    }),

    'Nat.*': (a, b) => ({ Nat: expectNat(a) * expectNat(b) }),
    'Nat./': (a, b) => ({ Nat: expectNat(a) / expectNat(b) }),
    'Nat.>': (a, b) => ({ Boolean: expectNat(a) > expectNat(b) }),
    'Nat.>=': (a, b) => ({ Boolean: expectNat(a) >= expectNat(b) }),
    'Nat.<': (a, b) => ({ Boolean: expectNat(a) < expectNat(b) }),
    'Nat.<=': (a, b) => ({ Boolean: expectNat(a) <= expectNat(b) }),
    'Nat.==': (a, b) => ({ Boolean: expectNat(a) == expectNat(b) }),
    'Nat.and': (a, b) => ({ Nat: expectNat(a) & expectNat(b) }),
    'Nat.or': (a, b) => ({ Nat: expectNat(a) | expectNat(b) }),
    'Nat.xor': (a, b) => ({ Nat: expectNat(a) ^ expectNat(b) }),
    'Nat.mod': (a, b) => ({ Nat: expectNat(a) % expectNat(b) }),
    'Nat.pow': (a, b) => ({
        Nat: Math.pow(expectNat(a), expectNat(b)),
    }),
    'Nat.shiftLeft': (a, b) => ({
        Nat: expectNat(a) << expectNat(b),
    }),
    'Nat.shiftRight': (a, b) => ({
        Nat: expectNat(a) >> expectNat(b),
    }),

    'Nat.drop': (a, b) => {
        if (b >= a) {
            return { Nat: 0 };
        } else {
            return { Nat: a - b };
        }
    },
    // , "Nat.sub": (2, SubN (Slot 1) (Slot 0))
    'Float.+': (a, b) => ({ Float: expectFloat(a) + expectFloat(b) }),
    'Float.-': (a, b) => ({ Float: expectFloat(a) - expectFloat(b) }),
    'Float.*': (a, b) => ({ Float: expectFloat(a) * expectFloat(b) }),
    'Float./': (a, b) => ({ Float: expectFloat(a) / expectFloat(b) }),
    'Float.<': (a, b) => ({
        Boolean: expectFloat(a) < expectFloat(b),
    }),
    'Float.<=': (a, b) => ({
        Boolean: expectFloat(a) <= expectFloat(b),
    }),
    'Float.>': (a, b) => ({
        Boolean: expectFloat(a) > expectFloat(b),
    }),
    'Float.>=': (a, b) => ({
        Boolean: expectFloat(a) >= expectFloat(b),
    }),
    'Float.==': (a, b) => ({
        Boolean: expectFloat(a) == expectFloat(b),
    }),

    'Universal.==': (one, two) => ({ Boolean: compare(one, two) === 0 }),
    'Universal.>': (one, two) => ({ Boolean: compare(one, two) > 0 }),
    'Universal.<': (one, two) => ({ Boolean: compare(one, two) < 0 }),
    'Universal.>=': (one, two) => ({ Boolean: compare(one, two) >= 0 }),
    'Universal.<=': (one, two) => ({ Boolean: compare(one, two) <= 0 }),
    'Universal.compare': (one, two) => ({ Int: compare(one, two) }),

    'Text.++': (a, b) => ({
        Text: expectText(a) + expectText(b),
    }),
    'Text.==': (a, b) => ({ Boolean: expectText(a) == expectText(b) }),
    'Text.!=': (a, b) => ({ Boolean: expectText(a) != expectText(b) }),
    'Text.<=': (a, b) => ({ Boolean: expectText(a) <= expectText(b) }),
    'Text.>=': (a, b) => ({ Boolean: expectText(a) >= expectText(b) }),
    'Text.>': (a, b) => ({ Boolean: expectText(a) > expectText(b) }),
    'Text.<': (a, b) => ({ Boolean: expectText(a) < expectText(b) }),
    'Text.take': (a, b) => ({
        Text: expectText(b).slice(0, expectNat(a)),
    }),
    'Text.drop': (a, b) => ({
        Text: expectText(b).slice(expectNat(b)),
    }),
    // , mk2 "Text.take" atn att (pure . T) (Text.take . fromIntegral)
    // , mk2 "Text.drop" atn att (pure . T) (Text.drop . fromIntegral)
    // , mk2 "Text.=="   att att (pure . B) (==)
    // , mk2 "Text.!="   att att (pure . B) (/=)
    // , mk2 "Text.<="   att att (pure . B) (<=)
    // , mk2 "Text.>="   att att (pure . B) (>=)
    // , mk2 "Text.>"    att att (pure . B) (>)
    // , mk2 "Text.<"    att att (pure . B) (<)
    'List.at': (a, l) => {
        a = expectNat(a);
        l = expectList(l);
        if (a < l.length) {
            return { PartialConstructor: [option_ref, 1, [l[a]]] };
        } else {
            return { Constructor: [option_ref, 0] };
        }
        // if a < &(l.len() as u64) {
        //     Value::PartialConstructor(
        //         option_ref,
        //         1,
        //         Vector::from([l[*a as usize]]),
        //     )
        // } else {
        //     Value::Constructor(option_ref, 0)
        // }
    },
    'List.cons': (value, l) => {
        l = expectList(l);
        l.unshift(value);
        return { Sequence: l };
    },
    'List.snoc': (l, value) => {
        l = expectList(l);
        l.push(value);
        return { Sequence: l };
    },
    'List.take': (n, l) => {
        n = expectNat(n);
        l = expectList(l);
        return { Sequence: l.slice(0, n) };
    },
    'List.drop': (n, l) => {
        n = expectNat(n);
        l = expectList(l);
        return { Sequence: l.slice(n) };
        // if *n as usize >= l.len() {
        //     Value::Sequence(Vector::new())
        // } else {
        //     let l = l.skip(*n as usize);
        //     Value::Sequence(l)
        // }
    },
    'List.++': (l0, l1) => {
        return { Sequence: expectList(l0).concat(expectList(l1)) };
    },
    // , mk2 "Bytes.++"  atbs atbs (pure . Bs) (<>)
    // , mk2 "Bytes.take" atn atbs (pure . Bs) (\n b -> Bytes.take (fromIntegral n) b)
    // , mk2 "Bytes.drop" atn atbs (pure . Bs) (\n b -> Bytes.drop (fromIntegral n) b)
    // , mk2 "Bytes.at" atn atbs pure $ \i bs ->
    //   IR.maybeToOptional (N . fromIntegral <$> Bytes.at (fromIntegral i) bs)
    // , mk2 "Float.atan2"     atf atf (pure . F) atan2
    // , mk2 "Float.logBase"   atf atf (pure . F) logBase

    // -- Power Functions
    // , mk2 "Float.pow"       atf atf (pure . F) (**)
    // -- Float Utils
    // , mk2 "Float.max"       atf atf (pure . F) max
    // , mk2 "Float.min"       atf atf (pure . F) min

    // , mk2 "Debug.watch" att at id (\t v -> putStrLn (Text.unpack t) *> pure v)
    // (a, b, c) => {
    //     throw new Error("ope")
    // },
};
