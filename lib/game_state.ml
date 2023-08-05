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

let update_state record state =
  match record with
  | Kill k ->
    let curr_game = IContext.find state.current_game state.games in
    let curr_game =
      if k.killer = "<world>"
      then (
        let killed = SContext.find k.killed curr_game.kills in
        { curr_game with kills = SContext.add k.killed (killed - 1) curr_game.kills })
      else (
        let killer = SContext.find k.killer curr_game.kills in
        { curr_game with kills = SContext.add k.killer (killer + 1) curr_game.kills })
    in
    let mod' =
      match MODContext.find_opt k.mean_of_death curr_game.kills_by_means with
      | Some value -> value + 1
      | None -> 1
    in
    { state with
      games =
        IContext.add
          state.current_game
          { curr_game with
            total_kills = curr_game.total_kills + 1
          ; kills_by_means = MODContext.add k.mean_of_death mod' curr_game.kills_by_means
          }
          state.games
    }
  | InitGame ->
    let games =
      IContext.add
        state.current_game
        { total_kills = 0
        ; kills_by_means = MODContext.empty
        ; players = []
        ; kills = SContext.empty
        }
        state.games
    in
    { state with games }
  | ShutdownGame -> { state with current_game = state.current_game + 1 }
  | ClientConnect id -> { state with plaryer_has_joined = Some id }
  | ClientUserInfoChanged (id, nickname) ->
    (match state.plaryer_has_joined with
     | Some id' when id = id' ->
       let game = IContext.find state.current_game state.games in
       let games =
         IContext.add
           state.current_game
           { game with
             players = nickname :: game.players
           ; kills = SContext.add nickname 0 game.kills
           }
           state.games
       in
       { state with plaryer_has_joined = None; games }
     | _ -> state)
;;

(* UNIT TESTS *)

(* TEST KILL EVENT *)
(* also tests the "killed by world" rule *)
let%test _ =
  let open Means_of_death in
  let state =
    { current_game = 0; plaryer_has_joined = None; games = IContext.empty }
    |> update_state InitGame
    |> update_state @@ ClientConnect 1
    |> update_state @@ ClientUserInfoChanged (1, "igor")
    |> update_state @@ ClientConnect 2
    |> update_state @@ ClientUserInfoChanged (2, "alexey")
    |> update_state
       @@ Kill { killer = "alexey"; killed = "igor"; mean_of_death = MOD_SHOTGUN }
    |> update_state
       @@ Kill { killer = "<world>"; killed = "igor"; mean_of_death = MOD_TRIGGER_HURT }
    |> update_state ShutdownGame
  in
  let game = IContext.find 0 state.games in
  let player_1 = SContext.find "igor" game.kills in
  let player_2 = SContext.find "alexey" game.kills in
  player_1 = -1 && player_2 = 1
;;
