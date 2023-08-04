type record =
  | Kill of kill
  | InitGame
  | ShutdownGame

and kill =
  { killer : string
  ; killed : string
  ; mean_of_death : Means_of_death.mod'
  }
