#!/bin/sh
set -ex
cargo build --lib --target wasm32-unknown-unknown --profile wasm --package vemfwasm
wasm-bindgen target/wasm32-unknown-unknown/wasm/vemfwasm.wasm --target web --no-typescript --out-dir wasm/pkg
wasm-opt -Os -o wasm/pkg/vemfwasm_bg.wasm wasm/pkg/vemfwasm_bg.wasm