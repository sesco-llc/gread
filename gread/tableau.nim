type
  Tableau* = object
    maxGenerations*: int   ## recommend termination after N generations
    maxPopulation*: int    ## evict via tournament due to over-crowding
    seedPopulation*: int   ## initial size of the population
    seedProgramSize*: int  ## initial bound on the size of random programs
    tournamentSize*: int   ## tournaments are comprised of N individuals
    useParsimony*: bool    ## whether to attempt to use parsimony
    sharingRate*: float    ## likelihood of sharing fittest members
    requireValid*: bool    ## the population ignores invalid additions

const
  defaultTableau* =
    Tableau(seedPopulation: 500, maxPopulation: 500,
            maxGenerations: 10_000_000, seedProgramSize: 100,
            requireValid: true, sharingRate: 3.0,
            tournamentSize: 10, useParsimony: on)
