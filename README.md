# gread

[![Test Matrix](https://github.com/disruptek/gread/workflows/CI/badge.svg)](https://github.com/disruptek/gread/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/gread?style=flat)](https://github.com/disruptek/gread/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.5.1%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/gread?style=flat)](#license)

Grammar-Evolving Algorithm Designer

## What Is It?

It's currently a very basic genetic programming system built to explore some
ideas for algorithm design, and specifically, grammatical evolution.

The architecture is generic to the user's choice of genome; while we're
currently experimenting with tree-based grammars, a future implementation
will support linear genome definitions, etc.

## Does It Evolve Nim Programs?

It's technically possible, but the Nim VM is fairly complex and has quite a few
bugs which make it difficult to use for ingesting code which is, at this early
stage, likely to be invalid.  The Nim VM is also relatively expensive to run.

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
Its syntax is more akin to the eventual linear representation that we target,
while affording all the same benefits of Lua.

## Installation

```
$ nimph clone disruptek/gread
```
or if you're still using Nimble like it's 2012,
```
$ nimble install https://github.com/disruptek/gread
```

## Documentation

Pretty light so far, sorry. Still experimenting with how best to structure the
implementation.

## License
GPLv3
