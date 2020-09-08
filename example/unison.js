const js = import('./node_modules/unison_wasm/unison_wasm.js');

export default async (data) => {
    const jsBridge = await js;
    const data = await data;
    const id = jsBridge.load(data);

    return {
        run: (term, args, handlers) => {
            //
        },
        runSync: (term, args, handlers) => {
            //
        },
        resume: (kont, arg, handlers) => {
            // we're in an async handler context, so don't expect resume to be sync?
            // would resume ... give you a value?
            // I guess we might think it would ... hmm ... ... ...
        },
        // NOTE: you can only do single-argument functions at this point.
        lambda: (partial, arg, handlers) => {
            //
        },
        lambdaSync: (partial, arg, handlers) => {
            //
        },
    };
};
