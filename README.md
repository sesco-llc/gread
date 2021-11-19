# gread

[![Test Matrix](https://github.com/disruptek/gread/workflows/CI/badge.svg)](https://github.com/disruptek/gread/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/gread?style=flat)](https://github.com/disruptek/gread/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.6.1%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/gread?style=flat)](#license)

Grammar-Evolving Algorithm Designer

## What Is It?

It's currently a very basic genetic programming system built to explore some
ideas for algorithm design, and specifically, grammatical evolution.

*The architecture is _generic_ to the user's choice of genome*; while we're
currently experimenting with tree-based grammars, we do technically support
linear genome definitions, etc.

This means that language support is generalized by a generic language type.
You must merely implement a few key functions in order to add novel language
support to the framework. Future support here will probably revolve around
tighter integration with htsparse (tree-sitter) so that if tree-sitter supports
your language, so will gread.

## So It's _Bring Your Own Language_?

Yes. We're initially testing against Lua, which is (unreasonably) powerful
as genetic programming languages go, but still quite performant. The
stack-based architecture also affords some implementation advantages over Nim's
register-based VM. Most importantly, Lua is likely to work.

## So You Evolve Lua Programs?

Practically, yes. Initial tests use [Fennel](https://fennel-lang.org/); a
lisp which compiles and runs in the Lua VM. An evolved Fennel program is thus
evaluated after being compiled into Lua.

## So You Evolve Fennel Programs!

Fennel contributes a syntax convenient for subtree swapping and yet still
provides the complexity of a macro system for evolving new grammar forms.

## Does It Evolve Nim Programs?

It's possible to evolve Nim programs with Gread. The Nim VM is fairly complex
and expensive to run. It also has quite a few crash bugs which make it
difficult to use for ingesting code produced at random.

## Installation

```
$ nimph clone disruptek/gread
```
or if you're still using Nimble like it's 2012,
```
$ nimble install https://github.com/disruptek/gread
```

## Usage

Check out the [examples](examples/) and note the following required compilation
switches:

- `--threads:on`
- `--gc:arc` or `--gc:orc`

## Debugging

### defines

- `greadProfile` profiles some interesting parts of the system
- `greadTreeSitter` enables creating programs from source code; you must have the tree-sitter library installed

## Documentation

[Pretty light so far, sorry. Still experimenting with how best to structure the
implementation.](https://disruptek.github.io/gread/gread.html)

## License
GPLv3
