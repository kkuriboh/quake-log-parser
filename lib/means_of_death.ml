open Angstrom

type mod' =
  | MOD_UNKNOWN
  | MOD_SHOTGUN
  | MOD_GAUNTLET
  | MOD_MACHINEGUN
  | MOD_GRENADE
  | MOD_GRENADE_SPLASH
  | MOD_ROCKET
  | MOD_ROCKET_SPLASH
  | MOD_PLASMA
  | MOD_PLASMA_SPLASH
  | MOD_RAILGUN
  | MOD_LIGHTNING
  | MOD_BFG
  | MOD_BFG_SPLASH
  | MOD_WATER
  | MOD_SLIME
  | MOD_LAVA
  | MOD_CRUSH
  | MOD_TELEFRAG
  | MOD_FALLING
  | MOD_SUICIDE
  | MOD_TARGET_LASER
  | MOD_TRIGGER_HURT
  | MOD_NAIL
  | MOD_CHAINGUN
  | MOD_PROXIMITY_MINE
  | MOD_KAMIKAZE
  | MOD_JUICED
  | MOD_GRAPPLE
[@@deriving compare]

type t = mod' [@@deriving compare]

let pp_mod = function
  | MOD_UNKNOWN -> "MOD_UNKNOWN"
  | MOD_SHOTGUN -> "MOD_SHOTGUN"
  | MOD_GAUNTLET -> "MOD_GAUNTLET"
  | MOD_MACHINEGUN -> "MOD_MACHINEGUN"
  | MOD_GRENADE -> "MOD_GRENADE"
  | MOD_GRENADE_SPLASH -> "MOD_GRENADE_SPLASH"
  | MOD_ROCKET -> "MOD_ROCKET"
  | MOD_ROCKET_SPLASH -> "MOD_ROCKET_SPLASH"
  | MOD_PLASMA -> "MOD_PLASMA"
  | MOD_PLASMA_SPLASH -> "MOD_PLASMA_SPLASH"
  | MOD_RAILGUN -> "MOD_RAILGUN"
  | MOD_LIGHTNING -> "MOD_LIGHTNING"
  | MOD_BFG -> "MOD_BFG"
  | MOD_BFG_SPLASH -> "MOD_BFG_SPLASH"
  | MOD_WATER -> "MOD_WATER"
  | MOD_SLIME -> "MOD_SLIME"
  | MOD_LAVA -> "MOD_LAVA"
  | MOD_CRUSH -> "MOD_CRUSH"
  | MOD_TELEFRAG -> "MOD_TELEFRAG"
  | MOD_FALLING -> "MOD_FALLING"
  | MOD_SUICIDE -> "MOD_SUICIDE"
  | MOD_TARGET_LASER -> "MOD_TARGET_LASER"
  | MOD_TRIGGER_HURT -> "MOD_TRIGGER_HURT"
  | MOD_NAIL -> "MOD_NAIL"
  | MOD_CHAINGUN -> "MOD_CHAINGUN"
  | MOD_PROXIMITY_MINE -> "MOD_PROXIMITY_MINE"
  | MOD_KAMIKAZE -> "MOD_KAMIKAZE"
  | MOD_JUICED -> "MOD_JUICED"
  | MOD_GRAPPLE -> "MOD_GRAPPLE"
;;

let p_mod =
  choice
    [ (string "MOD_UNKNOWN" >>| fun _ -> MOD_UNKNOWN)
    ; (string "MOD_SHOTGUN" >>| fun _ -> MOD_SHOTGUN)
    ; (string "MOD_GAUNTLET" >>| fun _ -> MOD_GAUNTLET)
    ; (string "MOD_MACHINEGUN" >>| fun _ -> MOD_MACHINEGUN)
    ; (string "MOD_GRENADE" >>| fun _ -> MOD_GRENADE)
    ; (string "MOD_GRENADE_SPLASH" >>| fun _ -> MOD_GRENADE_SPLASH)
    ; (string "MOD_ROCKET" >>| fun _ -> MOD_ROCKET)
    ; (string "MOD_ROCKET_SPLASH" >>| fun _ -> MOD_ROCKET_SPLASH)
    ; (string "MOD_PLASMA" >>| fun _ -> MOD_PLASMA)
    ; (string "MOD_PLASMA_SPLASH" >>| fun _ -> MOD_PLASMA_SPLASH)
    ; (string "MOD_RAILGUN" >>| fun _ -> MOD_RAILGUN)
    ; (string "MOD_LIGHTNING" >>| fun _ -> MOD_LIGHTNING)
    ; (string "MOD_BFG" >>| fun _ -> MOD_BFG)
    ; (string "MOD_BFG_SPLASH" >>| fun _ -> MOD_BFG_SPLASH)
    ; (string "MOD_WATER" >>| fun _ -> MOD_WATER)
    ; (string "MOD_SLIME" >>| fun _ -> MOD_SLIME)
    ; (string "MOD_LAVA" >>| fun _ -> MOD_LAVA)
    ; (string "MOD_CRUSH" >>| fun _ -> MOD_CRUSH)
    ; (string "MOD_TELEFRAG" >>| fun _ -> MOD_TELEFRAG)
    ; (string "MOD_FALLING" >>| fun _ -> MOD_FALLING)
    ; (string "MOD_SUICIDE" >>| fun _ -> MOD_SUICIDE)
    ; (string "MOD_TARGET_LASER" >>| fun _ -> MOD_TARGET_LASER)
    ; (string "MOD_TRIGGER_HURT" >>| fun _ -> MOD_TRIGGER_HURT)
    ; (string "MOD_NAIL" >>| fun _ -> MOD_NAIL)
    ; (string "MOD_CHAINGUN" >>| fun _ -> MOD_CHAINGUN)
    ; (string "MOD_PROXIMITY_MINE" >>| fun _ -> MOD_PROXIMITY_MINE)
    ; (string "MOD_KAMIKAZE" >>| fun _ -> MOD_KAMIKAZE)
    ; (string "MOD_JUICED" >>| fun _ -> MOD_JUICED)
    ; (string "MOD_GRAPPLE" >>| fun _ -> MOD_GRAPPLE)
    ]
;;
