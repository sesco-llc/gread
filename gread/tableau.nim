type
  Tableau* = object
    maxGenerations*: int
    maxPopulation*: int
    seedPopulation*: int
    seedProgramSize*: int
    tournamentSize*: int
    useParsimony*: bool
    sharingRate*: float
    requireValid*: bool

const
  defaultTableau* =
    Tableau(seedPopulation: 500, maxPopulation: 500,
            maxGenerations: 10_000_000, seedProgramSize: 5,
            requireValid: true,
            sharingRate: 2.0, tournamentSize: 6, useParsimony: on)
