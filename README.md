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

## Usage

Check out the [examples](examples/); the `average` example does not use threads
and serves as a simple and fast smoke-test.

The `lls` example performs a linear least-squares regression across threads; it
learns quickly but may not find a perfect solution before you lose interest.

Note the following required compilation switches:

- `--define:danger` (because otherwise it's debugging)
- `--define:useMalloc` (due to bugs in the Nim allocator)
- `--gc:arc` or `--gc:orc` (because we use CPS)
- `--panics:on` (because we use CPS)
- `--define:lunacyLuaJIT` (enables use of luajit)

## Tuning
<details>
  <summary>

**SPOILER**
This is as much an art as it is science, but we'll walk through the
metrics output and try to shed some light on how to interpret some of the
numbers...

  </summary>

Metrics are produced at specified generational intervals for each `Evolver` in
a `Cluster`, and each such instance has a `Core` identifier which is a simple
integer. `Evolvers` encompass unique populations of programs which may also
have unique fitness functions, datasets, virtual machines, and so on. There may
be multiple evolvers running on each thread but clusters generally only launch
`sysinfo.getNumTotalCores()` threads.

Here's sample metrics output from the `lls` example:

```
-5.0000[13]: (+ 2.0 (+ (+ x 2.0 ) 0.5 ) )
               core and thread: 2/11172
                  dataset size: 4
          virtual machine runs: 16681 (never reset)
            average vm runtime:   0.12 ms
         total population size: 500
            average age in pop: 5485
          validity rate in pop: 98.00%
           average valid score: -22.8557
          greatest of all time: -5.0000
           evolver cache count: 1770
           evolver cache usage: 81.04%
          average program size: 25
         program size variance: 146
          size of best program: 13
         parsimony coefficient: -0.0001
            insufficiency rate: 0.01%
           semantic error rate: 0.00%
             foreign influence: -
              immigration rate: 2.80%
          mapping failure rate: 41.79%
               best generation: 18633
             total generations: 20000
             invention recency: 6.83%
               generation time: 1.4514 ms
                evolution time: 66 sec
```

First, the `Score` of the fittest program discovered, followed by the program
length in brackets. The program length may be a measure of the genome size or
the abstract syntax tree, but it will not correspond to source code size or the
length of symbol names or constants. The program source code itself, Fennel in
this case, follows the score and length.

```
-5.0000[13]: (+ 2.0 (+ (+ x 2.0 ) 0.5 ) )
```

The core number and thread identifier as explained above.
```
               core and thread: 2/11172
```

A `SymbolSet` holds associations between symbols in the program, which do not
vary with program executions, and static input values, which may vary with each
`SymbolSet`.

The `dataset size` reflects the number of symbol sets presented to the `Evolver` for training purposes.

```
                  dataset size: 4
```

We keep a counter of invocations of the LuaVM and reproduce it here along with
the average runtime for all programs run on the virtual machine.

```
          virtual machine runs: 16681 (never reset)
            average vm runtime:   0.12 ms
```

Some population metrics are revealed, including the average age -- in
generations -- of programs that remain in the population, and the percentage of
population members which may be evaluated to produce valid results according
to the supplied `fitone()` function.

In the case of the `lls` example, the `fitone()` evaluates a program and
ensures that it produces a `float` that is not `nan`, `-inf` or `inf`.

Invalid programs cannot compete in tournament selection, so they aren't
terribly valuable -- a high validity rate is coincident with faster learning.

```
         total population size: 500
            average age in pop: 5485
          validity rate in pop: 98.00%
           average valid score: -22.8557
          greatest of all time: -5.0000
```

Some caching may be performed inside `Program` or `Evolver` objects and these
metrics can be useful to analyzing the degree to which programs must be
evaluated with multiple symbol sets in order to compete with one another. A
low cache usage figure is ideal, because it signifies efficient comparison of
programs without exhaustive evaluation.

```
           evolver cache count: 1770
           evolver cache usage: 81.04%
```

Program size is an important metric because it can signify bloat. Parsimony
below zero suggests that longer programs score more poorly, which the opposite
is true for parsimony above zero.

```
          average program size: 25
         program size variance: 146
          size of best program: 13
         parsimony coefficient: -0.0001
```

The insufficiency rate measures the percentage of programs which, when
evaluated on a single symbol set as in `fitone()`, do not produce an acceptable
`Score`.  The semantic error rate similarly measures errors raised by the LuaVM.

```
            insufficiency rate: 0.01%
           semantic error rate: 0.00%
```

Foreign influence is simply defined as whether the `fittest` program in the
population was sourced from a neighboring evolver in the cluster. If the
program was invented locally, this value will be `-`; otherwise, it will hold
the core number where the program was invented.

The take-away is that a program which was fittest in another population,
perhaps developed against a different dataset and with a different fitness
function, is nonetheless superior to locally-bred programs.

The immigration rate measures the percentage of programs in the population
which arrived from a neighboring evolver, as opposed to being invented locally.

This is a more general measure of the competitiveness of the local population
and, of course, the `sharingRate` across the cluster as a whole. A high value
here can indicate a lack of diversity; I like to see a 1-5% figure here for a
single-objective problem.

```
             foreign influence: -
              immigration rate: 2.80%
```

The mapping failure rate reflects occasions when offspring were produced
for which the genome was insufficient to encode the semantics of the entire
program. This figure reflects a lack of balance between non-terminal and
terminal nodes in the grammar, or perhaps a poor choice of mutation operators.
Ultimately, this is something that should be resolved in gread itself.

```
          mapping failure rate: 41.79%
```

The generational metrics tease out how recently the fittest individual was
invented, with the implication that continuous improvement is ideal.

```
               best generation: 18633
             total generations: 20000
             invention recency: 6.83%
```

We also measure the cumulative wall-clock time of the evolver's runtime, as
well as the average runtime of each generation since the last metrics were
reported.

```
               generation time: 1.4514 ms
                evolution time: 66 sec
```

</details>

## Debugging

### defines

- `greadPin` pin each thread to a core
- `greadCores` special desired maximum thread count
- `greadDebug` might enable some debugging here and there
- `greadProfile` profiles some interesting parts of the system

## Documentation

Nim's documentation generator does not work, but the code *is* documented.

## License
GPLv3
