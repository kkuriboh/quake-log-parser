open Batteries
open Statements
module SContext = Map.Make (String)
module IContext = Map.Make (Int)
module MODContext = Map.Make (Means_of_death)

type game =
  { total_kills : int
  ; kills_by_means : int MODContext.t
  ; players : string list
  ; kills : int SContext.t
  }

type state =
  { current_game : int
  ; plaryer_has_joined : int option
  ; games : game IContext.t
  }

let update_state s = function
  | Kill k ->
    let curr_game = IContext.find s.current_game s.games in
    let curr_game =
      if k.killer = "<world>"
      then (
        let killed = SContext.find k.killed curr_game.kills in
        { curr_game with kills = SContext.add k.killer (killed - 1) curr_game.kills })
      else (
        let killer = SContext.find k.killer curr_game.kills in
        { curr_game with kills = SContext.add k.killer (killer + 1) curr_game.kills })
    in
    let mod' =
      match MODContext.find_opt k.mean_of_death curr_game.kills_by_means with
      | Some value -> value + 1
      | None -> 1
    in
    { s with
      games =
        IContext.add
          s.current_game
          { curr_game with
            total_kills = curr_game.total_kills + 1
          ; kills_by_means = MODContext.add k.mean_of_death mod' curr_game.kills_by_means
          }
          s.games
    }
  | InitGame ->
    let games =
      s.games
      |> IContext.add
           s.current_game
           { total_kills = 0
           ; kills_by_means = MODContext.empty
           ; players = []
           ; kills = SContext.empty
           }
    in
    { s with games }
  | ShutdownGame -> { s with current_game = s.current_game + 1 }
  | ClientConnect id -> { s with plaryer_has_joined = Some id }
  | ClientUserInfoChanged (id, nickname) ->
    (match s.plaryer_has_joined with
     | Some id' when id = id' ->
       let game = IContext.find s.current_game s.games in
       let games =
         IContext.add
           s.current_game
           { game with
             players = nickname :: game.players
           ; kills = SContext.add nickname 0 game.kills
           }
           s.games
       in
       { s with plaryer_has_joined = None; games }
     | _ -> s)
;;
