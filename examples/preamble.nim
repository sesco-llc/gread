
const
  greadSeed {.intdefine.} = 0
  goodEnough = -0.01000     # termination condition
  llsMany {.intdefine.} = 10_000_000
  llsDuration {.intdefine.} = 300
  manyGenerations = llsMany
  statFrequency = 5_000
  llsGrammar = """
<start>            ::= <n-expr>

# numeric symbols
<n-value>          ::= "x"
<n-value>          ::= "1" | "2" | "4" | "8" | "16"

# n-arity operators upon numeric expressions
<n-nop>            ::= "+" | "-" | "*" | "/"

# perform n-arity operation on two or more numeric expressions
<n-expr-narity>    ::= ( <n-nop> <n-expr> <n-args> )
<n-expr-narity>    ::= <n-value>

# numeric arguments are arity/n
<n-args>           ::= <n-expr> | <n-expr> <n-args>
# numeric arguments are arity/2
#<n-args>           ::= <n-expr>
<n-args>           ::= <n-value>

# balancing numeric expressions
<n-expr>           ::= <n-expr-narity>
<n-expr>           ::= <n-value>
  """


  tourney = 0.01

# define the parameters for the evolvers
var tab = defaultTableau
tab -= {UseParsimony, RequireValid}
tab.seedProgramSize = 50
tab.seedPopulation = 200
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = max(2, int(tourney * tab.maxPopulation.float))
tab.sharingRate = 0.006
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
