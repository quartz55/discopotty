type t = string [@@deriving yojson]

let discord_epoch = 1420070400000

(* let t_of_yojson = function
  | `String s | `Intlit s -> Int64.of_string s
  | _ -> assert false *)
