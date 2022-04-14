(*
4000 `Unknown_error
4001 `Unknown_opcode
4002 `Decode_error
4003 `Not_authenticated
4004 `Authentication_failed
4005 `Already_authenticated
4007 `Invalid_seq
4008 `Rate_limited
4009 `Session_timed_out
4010 `Invalid_shard
4011 `Sharding_required
4012 `Invalid_API_version
4013 `Invalid_intents
4014 `Disallowed_intents
*)

type discord =
  [ `Unknown_error
  | `Unknown_opcode
  | `Decode_error
  | `Not_authenticated
  | `Authentication_failed
  | `Already_authenticated
  | `Invalid_seq
  | `Rate_limited
  | `Session_timed_out
  | `Invalid_shard
  | `Sharding_required
  | `Invalid_API_version
  | `Invalid_intents
  | `Disallowed_intents ]

type t = [ Websocket.Close_code.standard | discord ]

let is_discord = function #discord -> true | _ -> false
let is_std = function #Websocket.Close_code.standard -> true | _ -> false

let to_int = function
  | #Websocket.Close_code.standard as c -> Websocket.Close_code.to_int c
  | `Unknown_error -> 4000
  | `Unknown_opcode -> 4001
  | `Decode_error -> 4002
  | `Not_authenticated -> 4003
  | `Authentication_failed -> 4004
  | `Already_authenticated -> 4005
  | `Invalid_seq -> 4007
  | `Rate_limited -> 4008
  | `Session_timed_out -> 4009
  | `Invalid_shard -> 4010
  | `Sharding_required -> 4011
  | `Invalid_API_version -> 4012
  | `Invalid_intents -> 4013
  | `Disallowed_intents -> 4014

let of_close_code_exn = function
  | #Websocket.Close_code.standard as c -> c
  | `Other 4000 -> `Unknown_error
  | `Other 4001 -> `Unknown_opcode
  | `Other 4002 -> `Decode_error
  | `Other 4003 -> `Not_authenticated
  | `Other 4004 -> `Authentication_failed
  | `Other 4005 -> `Already_authenticated
  | `Other 4007 -> `Invalid_seq
  | `Other 4008 -> `Rate_limited
  | `Other 4009 -> `Session_timed_out
  | `Other 4010 -> `Invalid_shard
  | `Other 4011 -> `Sharding_required
  | `Other 4012 -> `Invalid_API_version
  | `Other 4013 -> `Invalid_intents
  | `Other 4014 -> `Disallowed_intents
  | `Other c -> failwith (Printf.sprintf "unknown voice close code: %d" c)

let pp =
  let pp' fmt = function
    (* TODO @quartz55 *)
    | `Unknown_error -> Format.fprintf fmt ""
    | `Unknown_opcode -> Format.fprintf fmt ""
    | `Decode_error -> Format.fprintf fmt ""
    | `Not_authenticated -> Format.fprintf fmt ""
    | `Authentication_failed -> Format.fprintf fmt ""
    | `Already_authenticated -> Format.fprintf fmt ""
    | `Invalid_seq -> Format.fprintf fmt ""
    | `Rate_limited -> Format.fprintf fmt ""
    | `Session_timed_out -> Format.fprintf fmt ""
    | `Invalid_shard -> Format.fprintf fmt ""
    | `Sharding_required -> Format.fprintf fmt ""
    | `Invalid_API_version -> Format.fprintf fmt ""
    | `Invalid_intents -> Format.fprintf fmt ""
    | `Disallowed_intents -> Format.fprintf fmt ""
  in
  fun fmt t ->
    match t with
    | #Websocket.Close_code.standard as c -> Websocket.Close_code.pp fmt c
    | #discord as t -> Format.fprintf fmt "(%d %a)" (to_int t) pp' t

let is_recoverable : t -> bool = function
  | `Authentication_failed | `Invalid_shard | `Sharding_required
  | `Invalid_API_version | `Invalid_intents | `Disallowed_intents
  | #Websocket.Close_code.standard ->
      false
  | _ -> true
