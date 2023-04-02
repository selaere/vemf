#!/bin/sh
set -ex
cargo build --lib --target wasm32-unknown-unknown --profile wasm --package vemfwasm
wasm-bindgen target/wasm32-unknown-unknown/wasm/vemfwasm.wasm --target web --no-typescript --out-dir wasm/pkg
wasm-opt -Os -o wasm/pkg/vemfwasm_bg.wasm wasm/pkg/vemfwasm_bg.wasm
esbuild --minify wasm/pkg/vemfwasm.js > wasm/pkg/vemfwasm-min.js
mv wasm/pkg/vemfwasm-min.js wasm/pkg/vemfwasm.js