# unison.rs

An experimental runtime for unison code, written in rust, compiled to wasm for use in the browser.

## Usage:

- download the [release binary](https://github.com/jaredly/unison.rs/releases/tag/release-2)
- run `unison.rs serve`
- go to `http://localhost:8080`, view the different namespaces that you've got, click on terms to "watch" them (including the ability to provide arguments to functions, as long as they're "primitives")
- run `unison` somewhere
- grab the web example (if you want to) `pull https://github.com/jaredly/unison-wasm-example .example`
- browse to `example.app` and click on it
- see that it's interactive!
- `cd example` and `edit counter` - in the scratch file, change `"Hello unison"` to something else
- save the scratch file, run `update` in unison, and see that the watcher auto-updates in your browser!
