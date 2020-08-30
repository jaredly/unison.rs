# unison.rs

An experimental parser + runtime for unison.

Note that this doesn't do any type-checking, and also does not write unison files.
The assumption is that if a term has been written to disk, that's because unison type-checked it, and it is correct.

The main goal of this project is to allow unison programs to run in the browser, through wasm. 🤞

## Contributing

It's all managed by cargo, so once you [have it set up](https://www.rust-lang.org/tools/install), this should be all you need:
```
$ cargo build
```

## Usage

### To run all ".test" terms found in a codebase:

```
$ env RUST_BACKTRACE=1 cargo run --release -- test ~/.unison/v1
```

### To evaluate a single term:

```
env RUST_BACKTRACE=1 cargo run --release --  ~/.unison/v1/terms/\#6l3pt38c2du7su2pecirpf2mjs7fv5prjr7utm956nb7j8u6msh0a3o8tihit595iudjijhm4u04jhoakr59qm3th3git62o4qsnvto/
```

 For verbose logging, add `RUST_LOG=uruson::ir_runtime,uruson::stack,uruson::ir_exec` before `RUST_BACKTRACE`.


## What about javascript compilation?

I think we could do a javascript transpilation setup that supported effects, but it would be a little gnarly — something similar to babel’s async/await transpilation setup — and if we want to be able to do with without sacrificing a ton of performance, we’d probably want to compile two different versions of any function that’s effect-polymorphic; one for the pure case (that doesn’t need the extra bookkeeping for effects) and one for the effect-ful case, that keeps track of stack variables, etc.