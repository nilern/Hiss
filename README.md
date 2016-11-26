# Hiss

A partial implementation of R7RS Scheme in Haskell.

## TODO

- [x] constants
    - [x] `quote`
- [x] variables
- [x] `set!`
- [x] `if`
- [x] `lambda`
- [x] calls
    - [x] builtins
    - [x] closures
    - [x] continuations
- [x] `apply`
- [x] `call/cc`
- [x] internals syntax
    - [x] `##intr#...`
    - [x] `##sf#...`
- [x] multiple values (`values` and `call-with-values`)
- [ ] `dynamic-wind`
- [ ] `define-record-type`
- [ ] exceptions
    - [ ] prompts & delimited continuations?
- [ ] macros
    - [ ] syntax objects
    - [ ] bootstrap expander (in Haskell)
    - [ ] hygienic expander
    - [ ] syntax-rules
    - [ ] libraries
    - [ ] programs
- [ ] REPL
- [ ] derived syntax (`let` etc.)
- [ ] library procedures and syntax
