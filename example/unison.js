const js = import('./node_modules/unison_wasm/unison_wasm.js');

const hashesForConstrName = (names) => {
    const hashesByName = {};
    Object.keys(names[2]).forEach((hash) => {
        names[2][hash].forEach((name) => (hashesByName[name.join('.')] = hash));
    });
    return hashesByName;
};

const convert_handlers = (handlers, typeNameHashes, names) => {
    const res = [];
    Object.keys(handlers).forEach((abilityName) => {
        const hash = typeNameHashes[abilityName];
        if (!hash) {
            console.log(typeNameHashes);
            throw new Error(`Hash not found for ability ${abilityName}`);
        }
        Object.keys(handlers[abilityName]).forEach((constrName) => {
            const v = handlers[abilityName][constrName];
            const idx = Object.keys(names[1][hash]).find((idx) => {
                if (
                    names[1][hash][idx].some(
                        (k) => k[k.length - 1] === constrName,
                    )
                ) {
                    return true;
                }
            });
            if (idx == null) {
                const allNames = [];
                Object.keys(names[1][hash]).forEach((idx) =>
                    names[1][hash][idx].forEach((name) =>
                        allNames.push(name.join('.')),
                    ),
                );
                throw new Error(
                    `Constructor not found for ability ${abilityName} : ${constrName} - found ${allNames.join(
                        ', ',
                    )}`,
                );
            }
            if (v.type === 'full') {
                res.push([hash, +idx, false, v.handler]);
            } else {
                res.push([hash, +idx, true, v]);
            }
        });
    });
    return res;
};

export default async (dataPromise, namesPromise) => {
    const jsBridge = await js;
    const data = await dataPromise;
    const names = await namesPromise;
    const id = jsBridge.load(data);
    const hashesByName = hashesForConstrName(names);

    return {
        run: (term, args, handlers) => {
            return jsBridge.run(
                id,
                term,
                args,
                convert_handlers(handlers, hashesByName, names),
            );
        },
        runSync: (term, args, handlers) => {
            return jsBridge.run_sync(
                id,
                term,
                args,
                convert_handlers(handlers, hashesByName, names),
            );
        },
        resume: (kont, arg, handlers) => {
            return jsBridge.resume(
                id,
                kont,
                arg,
                convert_handlers(handlers, hashesByName, names),
            );
            // we're in an async handler context, so don't expect resume to be sync?
            // would resume ... give you a value?
            // I guess we might think it would ... hmm ... ... ...
        },
        // NOTE: you can only do single-argument functions at this point.
        lambda: (partial, arg, handlers) => {
            throw new Error('No calling lambdas yet');
            //
        },
        lambdaSync: (partial, arg, handlers) => {
            throw new Error('No calling lambdas yet');
            //
        },
    };
};
