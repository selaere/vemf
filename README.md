vemf is my silly little programming language. it is sort of golfy and arraylike and tacit and esotericy and words words words etc etc.

documentation: <https://selaere.github.io/vemf/doc/docs.html>

try it online: <https://selaere.github.io/vemf/wasm/index.html>

## stability
the language is a lot more stable than it used to be, as most of the primitives have fallen in place neatly and i don't have many things in mind to add. however, things like the exact behavior of `ÖÜ` input/output and some of the weirder uncommon lettery functions (mostly dyadic `ïöäÆúù`, monadic `æÅ°`) are likely to be changed still, as is specific behavior with fill in certain functions. however, i believe it is mature enough to start using for some things. there is still no versioning scheme (this will change soon).

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
