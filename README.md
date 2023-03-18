vemf is my silly little programming language. it is sort of golfy and arraylike and tacit and esotericy and words words words etc etc.

documentation: <https://selaere.github.io/vemf/doc/docs.html>

try it online: <https://selaere.github.io/vemf/wasm/index.html>

## CAUTION CAUTION!!
this language, its interpreter and its documentation is in its early stages and everything is going to change. you should expect that any code you write today will be not working tomorrow. if you have feedback, ideas, bugs, thoughts, issues, things please let me know

## building

to build the interpreter/cli/repl, assuming you have cargo/rustup installed (remove the `--release` for a debug build):
```sh
$ cargo build --release --features=bin
```
you probably want to use `rlwrap` or equivalent in linux for an interactive repl.

to run the tests:
```sh
$ cargo test --release --lib
```
