type
  TableauFlags* = enum
    UseParsimony           ## whether to attempt to use parsimony
    RequireValid           ## the population ignores invalid additions
    EqualWeight            ## each data point is equally impactful to result

  Tableau* = object
    flags*: set[TableauFlags]  ## optional toggles
    maxGenerations*: int   ## recommend termination after N generations
    maxPopulation*: int    ## evict via tournament due to over-crowding
    seedPopulation*: int   ## initial size of the population
    seedProgramSize*: int  ## initial bound on the size of random programs
    tournamentSize*: int   ## tournaments are comprised of N individuals
    sharingRate*: float    ## likelihood of sharing members between cores

const
  defaultTableau* =
    Tableau(seedPopulation: 500, maxPopulation: 500,
            maxGenerations: 10_000_000, seedProgramSize: 200,
            sharingRate: 3.0, tournamentSize: 10,
            flags: {UseParsimony, RequireValid})

func contains*(tab: Tableau; flag: TableauFlags): bool =
  flag in tab.flags

func useParsimony*(tab: Tableau): bool {.deprecated: "use flags".} =
  UseParsimony in tab.flags

func requireValid*(tab: Tableau): bool {.deprecated: "use flags".} =
  RequireValid in tab.flags

func equalWeight*(tab: Tableau): bool {.deprecated: "use flags".} =
  EqualWeight in tab.flags
