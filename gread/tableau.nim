type
  TableauFlag* = enum
    UseParsimony           ## whether to attempt to use parsimony
    RequireValid           ## the population ignores invalid additions

  Tableau* = object
    flags*: set[TableauFlag]  ## optional toggles
    maxGenerations*: int   ## recommend termination after N generations
    maxPopulation*: int    ## evict via tournament due to over-crowding
    seedPopulation*: int   ## initial size of the population
    seedProgramSize*: int  ## initial bound on the size of random programs
    tournamentSize*: Positive   ## tournaments are comprised of N individuals
    sharingRate*: float    ## likelihood of sharing members between cores
    maxDurationInMs*: int  ## convenient termination metric

const
  defaultTableau* =
    Tableau(seedPopulation: 500, maxPopulation: 500,
            maxGenerations: 10_000_000, seedProgramSize: 200,
            sharingRate: 3.0, tournamentSize: 10, maxDurationInMs: 60_000,
            flags: {UseParsimony, RequireValid})

func contains*(tab: Tableau; flag: TableauFlag): bool =
  flag in tab.flags

func useParsimony*(tab: Tableau): bool {.deprecated: "use flags".} =
  UseParsimony in tab.flags

func requireValid*(tab: Tableau): bool {.deprecated: "use flags".} =
  RequireValid in tab.flags

func `+=`*(tab: var Tableau; value: set[TableauFlag]) =
  tab.flags = tab.flags + value

func `-=`*(tab: var Tableau; value: set[TableauFlag]) =
  tab.flags = tab.flags - value

func toggle*(tab: var Tableau; flag: TableauFlag) =
  if flag in tab.flags:
    tab.flags.excl flag
  else:
    tab.flags.incl flag

func toggle*(tab: var Tableau; flag: TableauFlag; included: bool) =
  if included:
    tab.flags.incl flag
  else:
    tab.flags.excl flag
