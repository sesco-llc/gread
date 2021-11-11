type
  Tableau* = object
    maxGenerations*: int
    maxPopulation*: int
    seedPopulation*: int
    seedProgramSize*: int
    tournamentSize*: int
    useParsimony*: bool
    sharingRate*: float

const
  defaultTableau* =
    Tableau(seedPopulation: 500, maxPopulation: 500,
            maxGenerations: 10_000_000, seedProgramSize: 5,
            sharingRate: 2.0, tournamentSize: 6, useParsimony: on)
