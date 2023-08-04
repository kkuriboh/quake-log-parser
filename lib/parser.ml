open Batteries
open Angstrom
open Means_of_death
open Statements

let ( << ) f g x = f (g x)
let ( <&> ) p1 p2 = p1 >>= fun x -> p2 >>| fun y -> x, y

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let non_numeric =
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false)
;;

let numeric =
  take_while1 (function
    | '1' .. '9' -> true
    | _ -> false)
;;

let alpha_numeric =
  take_while (function
    | 'a' .. 'z' | 'A' .. 'Z' | '1' .. '9' -> true
    | _ -> false)
;;

let timestamp = ws *> numeric <* char ':' <&> numeric
let nickname = many1 any_char >>| String.of_list

let kill_record =
  string "Kill:" *> ws *> skip_many (numeric *> ws) *> char ':' *> ws *> nickname
  <* ws
  <* string "killed"
  <* ws
  <&> nickname
  <* ws
  <* string "by"
  <* ws
  <&> p_mod
  >>| fun ((killer, killed), mean_of_death) -> Kill { killer; killed; mean_of_death }
;;

let init_game_record = string "InitGame:" *> ws *> many any_char >>| fun _ -> InitGame
let shutdown_game_record = string "ShutdownGame:" *> ws >>| fun _ -> ShutdownGame

let client_connect_record =
  string "ClientConnect:" *> ws *> numeric >>| fun id -> ClientConnect (int_of_string id)
;;

let client_user_info_changed_record =
  string "ClientUserInfoChanged:" *> ws *> numeric
  <* ws
  <* many any_char
  >>| fun id -> ClientConnect (int_of_string id)
;;

let records =
  timestamp
  *> ws
  *> choice
       [ kill_record
       ; client_connect_record
       ; client_user_info_changed_record
       ; init_game_record
       ; shutdown_game_record
       ]
;;

let apply str = parse_string ~consume:All records str
