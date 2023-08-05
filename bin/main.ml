open Batteries
open Quake_parser.Game_state

let () =
  let lines = File.lines_of "qgames.log" in
  let state =
    ref { current_game = 0; plaryer_has_joined = None; games = IContext.empty }
  in
  Enum.iter
    (fun line ->
      match Quake_parser.Parser.apply line with
      | Ok r -> state := update_state r !state
      | Error e -> print_endline e)
    lines;
  Format.eprintf "FINISHED %d" !state.current_game
;;
