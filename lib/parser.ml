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
    | '0' .. '9' -> true
    | _ -> false)
;;

let alpha_numeric =
  take_while (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false)
;;

let timestamp = ws *> numeric <* char ':' <&> numeric
let nickname = take_till (( = ) ' ')

let kill_record =
  string "Kill:" *> ws *> sep_by (char ' ') numeric *> char ':' *> ws *> nickname
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
  string "ClientUserinfoChanged:" *> ws *> numeric
  <* ws
  <* string "n\\"
  <&> take_till (( = ) '\\')
  <* many any_char
  >>| fun (id, nickname) -> ClientUserInfoChanged (int_of_string id, nickname)
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

(* UNIT TESTS *)

let%test _ =
  match apply " 20:54 Kill: 1022 2 22: <world> killed Isgalamido by MOD_TRIGGER_HURT" with
  | Ok (Kill record)
    when record.killer = "<world>"
         && record.killed = "Isgalamido"
         && record.mean_of_death = MOD_TRIGGER_HURT -> true
  | _ -> false
;;

let%test _ =
  match apply "  1:47 ClientConnect: 3" with
  | Ok (ClientConnect id) when id = 3 -> true
  | _ -> false
;;

let%test _ =
  match
    apply
      {|  1:47 ClientUserinfoChanged: 3 n\Isgalamido\t\0\model\uriel/zael\hmodel\uriel/zael\g_redteam\\g_blueteam\\c1\5\c2\5\hc\100\w\0\l\0\tt\0\tl\0|}
  with
  | Ok (ClientUserInfoChanged (id, nickname)) when id = 3 && nickname = "Isgalamido" ->
    true
  | _ -> false
;;

let%test _ =
  match
    apply
      {|1:47 InitGame: \sv_floodProtect\1\sv_maxPing\0\sv_minPing\0\sv_maxRate\10000\sv_minRate\0\sv_hostname\Code Miner Server\g_gametype\0\sv_privateClients\2\sv_maxclients\16\sv_allowDownload\0\bot_minplayers\0\dmflags\0\fraglimit\20\timelimit\15\g_maxGameClients\0\capturelimit\8\version\ioq3 1.36 linux-x86_64 Apr 12 2009\protocol\68\mapname\q3dm17\gamename\baseq3\g_needpass\0|}
  with
  | Ok InitGame -> true
  | _ -> false
;;

let%test _ =
  match apply " 1:47 ShutdownGame:" with
  | Ok ShutdownGame -> true
  | _ -> false
;;
