# unison.rs

An experimental parser + runtime for unison.

Note that this doesn't do any type-checking, and also does not write unison files.
The assumption is that if a term has been written to disk, that's because unison type-checked it, and it is correct.

The main goal of this project is to allow unison programs to run in the browser, through wasm. 🤞