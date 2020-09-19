const js = import('./node_modules/unison_wasm/unison_wasm.js');

const hashesForConstrName = (names) => {
    const hashesByName = {};
    Object.keys(names[2]).forEach((hash) => {
        names[2][hash].forEach((name) => (hashesByName[name.join('.')] = hash));
    });
    return hashesByName;
};

const hashesForTermName = (names) => {
    const hashesByName = {};
    Object.keys(names[0]).forEach((hash) => {
        names[0][hash].forEach((name) => (hashesByName[name.join('.')] = hash));
    });
    return hashesByName;
};

const convert_handlers = (handlers, typeNameHashes, names) => {
    const res = [];
    Object.keys(handlers).forEach((abilityName) => {
        const hash = typeNameHashes[abilityName];
        if (!hash) {
            // console.log(typeNameHashes);
            // throw new Error(`Hash not found for ability ${abilityName}`);
            return; // not needed
        }
        if (!names[1][hash]) {
            throw new Error(
                `No constructor data found for #${hash.slice(0, 10)}`,
            );
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

export const fetch = (dataUrl, namesUrl) => {
    return load(
        window.fetch(dataUrl).then((r) => r.text()),
        window.fetch(namesUrl).then((r) => r.json()),
    );
};

export const load = async (dataPromise, namesPromise) => {
    const jsBridge = await js;
    const data = await dataPromise;
    const names = await namesPromise;
    // console.log('have data', data.slice(0, 100));
    const id = jsBridge.load(data);
    const hashesByName = hashesForConstrName(names);
    const hashesByTermName = hashesForTermName(names);

    const getHash = (term) => {
        const hash = hashesByTermName[term];
        if (!hash) {
            console.log(Object.keys(hashesByTermName));
            throw new Error(`Term not found ${term}`);
        }
        return hash;
    };

    return {
        enableLogging: (prefix) =>
            prefix
                ? jsBridge.enable_logging_with_prefix(prefix)
                : jsBridge.enable_logging(),
        info: (term) => jsBridge.info(id, getHash(term)),
        canRunSync: (term, handlers) => {
            const hash = getHash(term);
            // const handlers = convert_handlers(handlers, hashesByName, names),
            return false; // TODO this should work
        },
        run: (term, args, handlers) => {
            const hash = getHash(term);
            const converted = convert_handlers(handlers, hashesByName, names);
            return jsBridge.run(id, hash, args, converted);
        },
        runSync: (term, args, handlers) => {
            return jsBridge.run_sync(
                id,
                getHash(term),
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
        },
        // NOTE: you can only do single-argument functions at this point.
        lambda: (partial, arg, handlers) => {
            return jsBridge.lambda(
                id,
                partial,
                arg,
                convert_handlers(handlers, hashesByName, names),
            );
        },
        lambdaSync: (partial, arg, handlers) => {
            throw new Error('No calling lambdas yet');
            //
        },
    };
};
