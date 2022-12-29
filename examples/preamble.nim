
const
  greadSeed {.intdefine.} = 0
  goodEnough = -0.10     # termination condition
  llsMany {.intdefine.} = 200_000
  manyGenerations = llsMany
  statFrequency = 10_000
  llsGrammar = """
    <start>        ::= <numexpr>
    <numexpr>      ::= ( <numbop> <numexpr> <numexpr> )
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numbop>       ::= "+" | "*" | "-" | "/"
    <value>        ::= "1.0" | "0.5" | "0.1" | "2.0"
    <value>        ::= "x"
  """
  tourney = 0.04

# define the parameters for the evolvers
var tab = defaultTableau
tab -= {UseParsimony}
tab -= {RequireValid, EqualWeight}
tab.seedProgramSize = 400
tab.seedPopulation = 1000
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = int(tourney * tab.maxPopulation.float)
tab.sharingRate = 0.0025
tab.maxGenerations = manyGenerations
