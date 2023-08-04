open Batteries
open Angstrom

let mapi f lst =
  let rec mapi i = function
    | [] -> []
    | x :: xs -> f x i @ mapi (i + 1) xs
  in
  mapi 0 lst
;;

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
  timestamp
  *> ws
  *> string "Kill:"
  *> ws
  *> skip_many (numeric *> ws)
  *> char ':'
  *> ws
  *> nickname
  <* ws
  <* string "killed"
  <* ws
  <&> nickname
  <* ws
  <* string "by"
  <* ws
  <&> nickname (* TODO: replace with means of death *)
;;
