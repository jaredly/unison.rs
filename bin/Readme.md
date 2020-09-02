# unison.rs

An experimental parser + runtime for unison.

Note that this doesn't do any type-checking, and also does not write unison files.
The assumption is that if a term has been written to disk, that's because unison type-checked it, and it is correct.

The main goal of this project is to allow unison programs to run in the browser, through wasm. 🤞

## Running unison programs with Wasm

### 1) Packaging up a term or namespace

```
$ unison.rs pack ~/.unison/ my.term
```

```
$ unison.rs pack-ns ~/.unison/ my.namespace
```

### 2) Running things

```js
import unison from 'unison-wasm';
import workspace from 'raw-loader!./my-workspace.bin';

unison.load(workspace).then(workspace => {
    // this is a nested object, where the functions are available
    // where a term and a namespace have the same name ... then ...
    // workspace.run()
    workspace.get('one.two.three')
    workspace.run('one.two.three', 2, 3, 4)
})

```






### 1) Packaging up the terms
There are currently two options: pack a single term, or pack up the whole workspace.

To pack a single term, run
```sh
$ unison.rs pack ~/.unison/v1/terms/[the hash] my-term.bin
```
This will create `my-term.bin` and `my-term.bin.json` (which contains a mapping of hashes to names, for debugging).

To pack a whole workspace, run
```sh
$ unison.rs pack-all ~/.unison/v1/terms workspace.bin
```

### 2) Running your code
Currently webpack is required to make everything work. See the `example` directory for a full working example.

```js
import unison from 'unison-wasm';
import workspace from 'raw-loader!./my-workspace.bin';

unison.load(workspace).then(workspace => {
    workspace.run()
})

```


## Usage




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