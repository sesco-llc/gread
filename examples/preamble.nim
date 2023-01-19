
const
  greadSeed {.intdefine.} = 0
  goodEnough = -0.40000     # termination condition
  llsMany {.intdefine.} = 100_000
  manyGenerations = llsMany
  statFrequency = 5_000
  llsGrammar = """
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
tab -= {UseParsimony}
tab -= {RequireValid, EqualWeight}
tab.seedProgramSize = 500
tab.seedPopulation = 100
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = int(tourney * tab.maxPopulation.float)
tab.sharingRate = 0.030
tab.maxGenerations = manyGenerations
