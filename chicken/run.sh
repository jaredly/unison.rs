#!/bin/bash
cargo run --release -- pack-all-chicken ~/.unison/v1/terms runtime_tests ./runtime_tests.scm && /Applications/Racket\ v7.9/bin/racket pretty.rkt
