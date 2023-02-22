vemf is a programming language. it is sort of golfy and arraylike and tacit and words words words etc etc.

## CAUTION CAUTION!!
this language and interpreter is in its early stages and _everything_ is bound to change. you should expect that any code you write today will be not working tomorrow. if you have feedback, ideas, things, bugs, thoughts, issues please let me know

## building

to build the interpreter/cli/repl, assuming you have cargo/rustup installed (remove the `--release` for a debug build):
```sh
$ cargo build --release --features lib
```
you probably want to use `rlwrap` or equivalent in linux for an interactive repl.

to run the tests:
```sh
$ cargo test --lib
```
