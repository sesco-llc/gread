
const
  greadSeed {.intdefine.} = 0
  goodEnough = -0.40000     # termination condition
  llsMany {.intdefine.} = 10_000_000
  llsDuration {.intdefine.} = 30
  manyGenerations = llsMany
  statFrequency = 5_000
  llsGrammar = """
    # a comment
    <start>        ::= <numexpr>
    <varargs>      ::= <numexpr> | <numexpr> <varargs>
    <numexpr>      ::= ( <numbop> <numexpr> <varargs> )
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numbop>       ::= "+" | "*" | "-" | "/"
    <value>        ::= "1.0" | "2.0" | "0.5" | "0.1"
    <value>        ::= "x"
  """
  tourney = 0.04

# define the parameters for the evolvers
var tab = defaultTableau
tab -= {UseParsimony, RequireValid}
tab.seedProgramSize = 500
tab.seedPopulation = 100
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = int(tourney * tab.maxPopulation.float)
tab.sharingRate = 0.030
tab.maxGenerations = manyGenerations

let
  cores =
    when not defined(release) and greadSeed != 0:
      1
    else:
      processors
      #max(1, getNumTotalCores())

var gram: Grammar
initFennelGrammar(gram, llsGrammar)

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
var dataset: seq[Locals]
for (x, y) in data.items:
  dataset.add:
    initLocals [("x", x.toLuaValue), ("y", y.toLuaValue)]

proc fitone(fnl: Fennel; locals: Locals; p: var FProg): Option[LuaValue] =
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
