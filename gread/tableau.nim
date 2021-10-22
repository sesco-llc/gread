type
  Tableau* = object
    maxGenerations*: int
    maxPopulation*: int
    seedPopulation*: int
    seedProgramSize*: int
    tournamentSize*: int
    useParsimony*: bool

const
  defaultTableau* =
    Tableau(seedPopulation: 1000, maxPopulation: 1000,
            maxGenerations: 200_000, seedProgramSize: 5,
            tournamentSize: 6, useParsimony: on)
