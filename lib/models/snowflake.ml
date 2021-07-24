type t = int64

let of_string = Int64.of_string

let to_string = Int64.to_string

let t_of_yojson : Yojson.Safe.t -> t = function
  | `String s | `Intlit s -> of_string s
  | _ -> assert false

let yojson_of_t t = `String (to_string t)

let discord_epoch = 1420070400000L

let timestamp t = CCInt64.((t lsr 22) + discord_epoch)

let worker_id t = CCInt64.((t land 0x3E0000L) lsr 17 |> to_int)

let process_id t = CCInt64.((t land 0x1F000L) lsr 12 |> to_int)

let increment t = CCInt64.(t land 0xFFFL |> to_int)

let pp fmt t =
  Format.fprintf fmt
    "@[(%Ld@[(ts=%Ld)@] @[(wid=%d)@] @[(pid=%d)@] @[(inc=%d)@])@]" t
    (timestamp t) (worker_id t) (process_id t) (increment t)

let compare l r = Int64.compare (timestamp l) (timestamp r)

let ( = ) = Int64.equal

let ( <> ) a b = not (a = b)
