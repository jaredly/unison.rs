# unison.rs

An experimental runtime for unison code, written in rust, compiled to wasm for use in the browser.

## "Counter" interactive web example:

- in ucm, run `pull https://github.com/jaredly/unison-web-example .app_test`
- clone this repo (you'll need to have rust installed), and run `cargo run --release -- pack-all ~/.unison/v1/terms ./example/data/all.bin`
- in the `example` directory, run `yarn` to install dependencies, then `yarn serve`
- open `http://localhost:8080`!

![screenshot](./screenshot.png)


## Dreamcode:

- download the binary, run `unison_rs pack-watch .my-app data/my-app.bin`
- run `yarn serve`
- hmmmmmmmm what if I have a quickstart that doesn't rely on webpack? because we've got everything prebuilt.... hmm.... yes.... probably want a rust server folks, yesiree
