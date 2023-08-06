open Batteries
open Statements
module SContext = Map.Make (String)
module IContext = Map.Make (Int)
module MODContext = Map.Make (Means_of_death)

type game =
  { total_kills : int
  ; kills_by_means : int MODContext.t
  ; players : (string * int) list
  ; kills : int SContext.t
  }

type state =
  { current_game : int
  ; player_has_joined : int option
  ; games : game IContext.t
  }

let rec replace to_replace value = function
  | [] -> []
  | x :: xs -> if x = to_replace then value :: xs else x :: replace to_replace value xs
;;

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
  | ClientConnect id -> { state with player_has_joined = Some id }
  | ClientUserInfoChanged (id, nickname) ->
    let game = IContext.find state.current_game state.games in
    (match state.player_has_joined with
     | Some id' when id = id' ->
       let player =
         (* since players can reconnect with different IDs and different nicknames,
          * that's all we can do. we are matching on the nickname because most
          * of the times the ID has certainly been changed *)
         List.find_opt (fun (nickname', _) -> nickname' = nickname) game.players
       in
       let game =
         match player with
         | Some _ -> game
         | None ->
           { game with
             players = (nickname, id) :: game.players
           ; kills = SContext.add nickname 0 game.kills
           }
       in
       let games = IContext.add state.current_game game state.games in
       { state with player_has_joined = None; games }
     | _ ->
       let player =
         List.find_opt
           (fun (nickname', id') ->
             (id' = id && nickname' <> nickname) || (id' <> id && nickname' = nickname))
           game.players
       in
       (match player with
        | Some player ->
          let old_name, old_id = player in
          let curr_kill_count = SContext.find old_name game.kills in
          let kills =
            game.kills
            |> SContext.remove old_name
            |> SContext.add nickname curr_kill_count
          in
          let players = replace (old_name, old_id) (nickname, id) game.players in
          let game = { game with players; kills } in
          { state with games = IContext.add state.current_game game state.games }
        | None -> state))
  | Ignore -> state
;;

(* UNIT TESTS *)

(* TEST: KILL EVENT *)
(* also tests the "killed by <world>" rule *)
let%test _ =
  let open Means_of_death in
  let state =
    { current_game = 0; player_has_joined = None; games = IContext.empty }
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

(* TEST: NAME BEING CHANGED *)
let%test _ =
  let state =
    { current_game = 0; player_has_joined = None; games = IContext.empty }
    |> update_state InitGame
    |> update_state @@ ClientConnect 1
    |> update_state @@ ClientUserInfoChanged (1, "igor")
    |> update_state @@ ClientUserInfoChanged (1, "alexey")
    |> update_state ShutdownGame
  in
  let game = IContext.find 0 state.games in
  let igor = SContext.find_opt "igor" game.kills in
  let alexey = SContext.find_opt "alexey" game.kills in
  Option.is_none igor && Option.is_some alexey
;;

(* TEST: RECONNECTION *)
let%test _ =
  let open Means_of_death in
  let state =
    { current_game = 0; player_has_joined = None; games = IContext.empty }
    |> update_state InitGame
    |> update_state @@ ClientConnect 1
    |> update_state @@ ClientUserInfoChanged (1, "igor")
    |> update_state @@ ClientConnect 2
    |> update_state @@ ClientUserInfoChanged (2, "alexey")
    |> update_state
       @@ Kill { killer = "alexey"; killed = "igor"; mean_of_death = MOD_SHOTGUN }
    |> update_state
       @@ Kill { killer = "<world>"; killed = "igor"; mean_of_death = MOD_TRIGGER_HURT }
    (* reconnected with id 3 *)
    |> update_state @@ ClientUserInfoChanged (3, "igor")
    (* changed nickname right after *)
    |> update_state @@ ClientUserInfoChanged (3, "jonathan")
    |> update_state ShutdownGame
  in
  let game = IContext.find 0 state.games in
  let player = SContext.find "jonathan" game.kills in
  let not_player = SContext.find_opt "igor" game.kills in
  player = -1 && Option.is_none not_player
;;
