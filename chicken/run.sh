#!/bin/bash
cargo run --release -- pack-all-chicken ~/.unison/v1/terms runtime_tests.abilities ./runtime_tests_abilities.scm && /Applications/Racket\ v7.9/bin/racket pretty.rkt
