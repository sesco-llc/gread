
const
  greadSeed {.intdefine.} = 0
  goodEnough = -0.10000     # termination condition
  llsMany {.intdefine.} = 100_000
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
  tourney = 0.03

# define the parameters for the evolvers
var tab = defaultTableau
tab -= {UseParsimony}
tab -= {RequireValid, EqualWeight}
tab.seedProgramSize = 300
tab.seedPopulation = 300
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = int(tourney * tab.maxPopulation.float)
tab.sharingRate = 0.004
tab.maxGenerations = manyGenerations
