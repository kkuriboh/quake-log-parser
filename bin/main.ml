open Batteries
open Quake_parser.Game_state

let () =
  let lines = File.lines_of "qgames.log" in
  let state =
    ref { current_game = 0; player_has_joined = None; games = IContext.empty }
  in
  Enum.iteri
    (fun idx line ->
      match Quake_parser.Parser.apply line with
      | Ok r -> state := update_state r !state
      | Error e -> failwith @@ Format.sprintf "%d: %s\n" (idx + 1) e)
    lines;
  Format.eprintf "FINISHED %d\n" !state.current_game
;;
