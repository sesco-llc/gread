when not defined(greadFast):
  {.error: "needs --define:greadFast".}
when not compileOption"threads":
  {.error: "needs --threads:on".}

import std/hashes
import std/options
import std/os
import std/packedsets
import std/random
import std/sets
import std/times

import gread
import gread/fennel except variance

import pkg/sysinfo
import pkg/cps
import pkg/lunacy
import pkg/loony
import pkg/adix/lptabz

const
  goodEnough = -0.10     # termination condition
  statFrequency = 5000  # report after this many generations
  llsGrammar = """
    <start>        ::= <numexpr>
    <numexpr>      ::= ( <numbop> <numexpr> <numexpr> )
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numbop>       ::= "+" | "*" | "-" | "/"
    <value>        ::= "1.0" | "0.5" | "0.1" | "2.0"
    <value>        ::= "x"
  """

var gram: Grammar
initFennelGrammar(gram, llsGrammar)

# you can adjust these weights to change mutation rates
let operators = {
  geCrossover[Fennel, LuaValue]:        1.0,
  geMutation[Fennel, LuaValue]:         1.0,
  #subtreeXover[Fennel, LuaValue]:       1.0,
  #randomSubtreeXover[Fennel, LuaValue]: 1.0,
  randomCrossover[Fennel, LuaValue]:    1.0,
}

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
var dataset: seq[Locals]
for (x, y) in data.items:
  dataset.add:
    initLocals [("x", x.toLuaValue), ("y", y.toLuaValue)]

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[LuaValue] =
  ## given a datapoint, run the program and return the residual
  let s = evaluate(fnl, p, locals)
  if s.isValid:
    result =
      some:
        toLuaValue -abs(locals["y"].toFloat - s.toFloat)

proc fitmany(fnl: Fennel; iter: iterator(): (ptr Locals, ptr LuaValue);
             p: FProg): Option[LuaValue] =
  ## given several residuals, return the sum of squares
  var results = newSeqOfCap[float](data.len)
  for locals, s in iter():
    if s[].isValid:
      results.add s[]
    else:
      return none LuaValue
  if results.len > 0:
    let s = toLuaValue -ss(results)
    if s.isValid:
      result = some s

when isMainModule:
  import gread/cluster

  randomize()

  # define the parameters for the evolvers
  var tab = defaultTableau
  tab.flags = {UseParsimony, EqualWeight}
  tab.seedProgramSize = 400
  tab.seedPopulation = 400
  tab.maxPopulation = 400
  tab.tournamentSize = int(0.03 * tab.maxPopulation.float)
  tab.sharingRate = 0.15
  tab.maxGenerations = 1_000_000

  # the main loop monitors inventions
  proc main(work: Work; inputs, outputs: LoonyQueue[FProg]) =
    # create a population to monitor new inventions
    let fnl = newFennel()
    var monitor = tab
    monitor.maxPopulation = 10
    var evo: Evolver[Fennel, LuaValue]
    initEvolver(evo, fnl, monitor)
    evo.strength = strength
    evo.grammar = gram
    evo.operators = operators
    evo.dataset = dataset
    evo.fitone = fitone
    evo.fitmany = fitmany
    evo.population =
      newPopulation[Fennel](monitor.maxPopulation, core = evo.core)

    var seen: HashSet[Hash]
    let et = getTime()
    #var seen: PackedSet[Hash]
    while true:
      let p = pop inputs
      if p.isNil:
        sleep 250
      else:
        if p.isValid and not p.zombie and not seen.containsOrIncl(p.hash):
          # FIXME: this shouldn't be necessary
          let p = clone p
          p.score = strength(evo)(get evo.score(p))
          while p.isValid:
            evo.makeRoom()
            if not p.isValid:
              break
            evo.population.add p
            when defined(greadFast):
              maybeResetFittest(evo.population, p)
            let best = evo.fittest
            if best.isSome and best.get.hash == p.hash:
              dumpScore(fnl, p)
              if p.score > goodEnough:
                echo "winner, winner, chicken dinner: ", p.score
                echo "last generation: ", p.generation, " secs: ", (getTime() - et).inSeconds
                quit 0
            break
        if p.isValid:
          push(outputs, p)

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, operators = operators,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           strength = fennel.strength, stats = statFrequency)

  let cores = max(1, getNumTotalCores())
  for core in 1..cores:
    args.rng = some initRand()
    clump.boot(whelp worker(args), args.core)
    clump.redress args

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
