# gread

[![Test Matrix](https://github.com/disruptek/gread/workflows/CI/badge.svg)](https://github.com/disruptek/gread/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/gread?style=flat)](https://github.com/disruptek/gread/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.6.1%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/gread?style=flat)](#license)

Grammar-Evolving Algorithm Designer

## What Is It?

It's currently a basic genetic programming system built to explore some ideas
for algorithm design, and specifically, grammatical evolution.

*The architecture is _generic_ to the user's choice of language.*

## So It's _Bring Your Own Language_?

Yes; the genome is merely a blob of binary data and language support is
generalized by a generic language type.  You specify the grammar for your
language using a form of BNF notation in a string.

We're initially testing against Lua, which is (unreasonably) powerful as
genetic programming languages go, but still quite performant.

## So You Evolve Lua Programs?

Yes, you can evolve Lua programs by importing `gread/lua` and defining the Lua
grammar you wish to use.

I'm (still) finding [Fennel](https://fennel-lang.org/) easier; it's a lisp
which compiles and runs in the Lua VM.

## So You Evolve Fennel Programs!

An evolved Fennel program is evaluated after being compiled into Lua. This is a
small performance penalty that we will design out eventually.

## Does It Evolve Nim Programs?

It's possible to evolve Nim programs with Gread. The Nim VM is fairly complex
and expensive to run. Also, there is presently no tree-sitter support for Nim's
grammar.

## What About Other Languages?

To add novel language support to the framework, you merely implement a few key
functions that Gread will exploit.

Future support here will probably revolve around tighter integration with
htsparse (tree-sitter) so that if tree-sitter supports your language, so will
Gread.

## Installation

You probably need some flavor of Linux for this, but it might work under OSX.

Gread requires the tree-sitter library for parsing extant programs into AST;
this was not always the case, but going forward, I think it will be more
valuable as a full dependency we can rely upon, rather than an optional one.

```
$ nimph clone sesco-llc/gread
```
or if you're still using Nimble like it's 2012,
```
$ nimble install https://github.com/sesco-llc/gread
```

## Threads

Gread supports threads and you _really_ want to exploit this because it can
produce _massive_ speed-ups in evolution. However, there's a problem: Nim's
shared heap and reference counting conspire to crash programs built against
mainline version-1-6 and devel branches.

You'll need the atomic reference counting implementation
in `lib/system/atomics.nim` and `lib/system.nim` from
https://github.com/nim-works/nimskull/pull/88 in order to exploit threading
under Gread without undefined behavior leading to crashes or memory corruption.

## Usage

Check out the [examples](examples/); the `average` example does not use threads
and serves as a simple and fast smoke-test.

The `lls` example performs a linear least-squares regression across threads; it
learns quickly but may not find a perfect solution before you lose interest.

Note the following required compilation switches:

- `--define:danger` (because otherwise it's debugging)
- `--define:useMalloc` (due to bugs in the Nim allocator)
- `--threads:on` (to evolve disparate populations across CPU cores)
- `--gc:arc` or `--gc:orc` (because we use CPS and Loony)
- `--panics:on` (because we use CPS)
- `--define:lunacyLuaJIT` (enables use of luajit)

## Debugging

### defines

- `greadProfile` profiles some interesting parts of the system
- `greadWrapping` enables wrapping of the genome during mapping _(not fully implemented yet)_

## Documentation

[A little light so far, sorry. Still experimenting with how best to structure the implementation.](https://sesco-llc.github.io/gread/gread.html)

## License
GPLv3
