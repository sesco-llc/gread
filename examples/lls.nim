import std/times
import std/packedsets
import std/options
import std/os
import std/osproc
import std/hashes
import std/random
import std/math

import gread
import gread/fennel except variance

import pkg/cps
import pkg/lunacy
import pkg/loony
import pkg/adix/lptabz

const
  goodEnough = -1.00     # termination condition
  statFrequency = 10000  # report after this many generations
  llsGrammar = """
    <start>        ::= <numexpr>
    <numexpr>      ::= ( <numbop> <numexpr> <numexpr> )
    <numexpr>      ::= <value>
    <numbop>       ::= "+" | "*" | "-" | "/"
    <value>        ::= "1.0" | "0.5" | "0.1" | "2.0"
    <value>        ::= "x"
  """

var gram: Grammar[Fennel]
initGrammar(gram, llsGrammar)

# you can adjust these weights to change mutation rates
let operators = {
  geCrossover[Fennel, LuaValue]:    50.0,
  geMutation[Fennel, LuaValue]:     25.0,
  randomCrossover[Fennel, LuaValue]: 5.0,
}

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
var dataset: seq[Locals]
for (x, y) in data.items:
  dataset.add:
    initLocals [("x", x.toLuaValue), ("y", y.toLuaValue)]

# preparing a map between inputs and ideal output for reporting reasons
var training: seq[(Locals, Score)]
for locals in dataset.items:
  training.add (locals, Score locals["y"].toFloat)

proc fenfit(inputs: Locals; output: LuaValue): Score =
  ## fenfit gates program output such that producing a NaN will terminate
  ## scoring of the program early, and mark the program as invalid
  if output.kind == TNumber:
    output.toFloat
  else:
    NaN

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[Score] =
  ## given a datapoint, run the program and return the residual
  let s = evaluate(fnl, p, locals, fenfit)
  if s.isValid:
    result =
      some:
        Score -abs(locals["y"].toFloat - s.float)

proc fitmany(fnl: Fennel; data: openArray[(Locals, Score)];
             p: FProg): Option[Score] =
  ## given several residuals, return the sum of squares
  var results = newSeqOfCap[float](data.len)
  for locals, s in data.items:
    if s.isValid:
      results.add s
    else:
      return none Score
  if results.len > 0:
    let s = Score -ss(results)
    if s.isValid:
      result = some s

when isMainModule:
  import gread/cluster

  randomize()

  # the main loop monitors inventions
  proc main(work: Work; inputs, outputs: LoonyQueue[FProg]) =
    let fnl = newFennel()
    var best: FProg
    let et = getTime()
    var seen: PackedSet[Hash]
    while true:
      let p = pop inputs
      if p.isNil:
        sleep 250
      else:
        #if FinestKnown in p.flags:
        if not seen.containsOrIncl(p.hash):
          #echo "winner ", p.score, " ", p.hash, " core ", p.core
          if best.isNil or not best.score.isValid or best.score < p.score:
            best = p
            dumpPerformance(fnl, best, training, fenfit, samples = 1)
            if best.score > goodEnough:
              echo "winner, winner, chicken dinner: ", best.score
              echo "last generation: ", p.generation, " secs: ", (getTime() - et).inSeconds
              quit 0
              break
        push(outputs, p)

  # now setup the workers with their unique populations, etc.
  var tab = defaultTableau
  tab.useParsimony = false
  tab.seedProgramSize = 200
  tab.seedPopulation = 500
  tab.maxPopulation = 500
  tab.tournamentSize = 10
  tab.sharingRate = 3.0
  tab.maxGenerations = 200_000
  tab.requireValid = true

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, operators = operators,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           stats = statFrequency)

  for core in 0..<countProcessors():
    clump.boot(whelp worker(args), args.core)
    clump.redress args

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
