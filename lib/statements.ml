type record =
  | Kill of kill
  | ClientConnect of int
  | ClientUserInfoChanged of int * string
  | InitGame
  | ShutdownGame

and kill =
  { killer : string
  ; killed : string
  ; mean_of_death : Means_of_death.mod'
  }
