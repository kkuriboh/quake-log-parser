open Game_state
open Means_of_death
open Batteries

let id x = x

let stringfy_game idx game =
  Format.sprintf
    {|"game_%d": {
        "total_kills": %d,
        "players": [%s],
        "kills": {
            %s
        },
        "kills_by_means": {
            %s
        }
    }|}
    (idx + 1)
    game.total_kills
    (game.players
     |> List.map (fun (name, _) -> Format.sprintf {|"%s"|} name)
     |> String.join ", ")
    (game.kills
     |> SContext.to_seq
     |> Seq.map (fun (key, value) -> Format.sprintf {|"%s": %d|} key value)
     |> List.of_seq
     |> String.join ", ")
    (game.kills_by_means
     |> MODContext.to_seq
     |> Seq.map (fun (key, value) -> Format.sprintf {|"%s": %d|} (pp_mod key) value)
     |> List.of_seq
     |> String.join ",\n            ")
;;

let state_to_json s =
  Format.sprintf
    "{\n    %s\n}"
    (s.games
     |> IContext.to_seq
     |> Seq.map (fun (key, value) -> stringfy_game key value)
     |> List.of_seq
     |> String.join ",\n    ")
;;
