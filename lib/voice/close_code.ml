module Websocket = Disco_core.Websocket

type discord =
  [ `Unknown_op
  | `Invalid_payload
  | `Not_authenticated
  | `Authentication_failed
  | `Already_authenticated
  | `Invalid_session
  | `Session_timeout
  | `Server_not_found
  | `Unknown_protocol
  | `Disconnected
  | `Voice_server_crashed
  | `Unknown_encryption_mode ]

type t = [ Websocket.Close_code.standard | discord ]

let is_discord = function #discord -> true | _ -> false

let is_std = function #Websocket.Close_code.standard -> true | _ -> false

let to_int = function
  | #Websocket.Close_code.standard as c -> Websocket.Close_code.to_int c
  | `Unknown_op -> 4001
  | `Invalid_payload -> 4002
  | `Not_authenticated -> 4003
  | `Authentication_failed -> 4004
  | `Already_authenticated -> 4005
  | `Invalid_session -> 4006
  | `Session_timeout -> 4009
  | `Server_not_found -> 4011
  | `Unknown_protocol -> 4012
  | `Disconnected -> 4014
  | `Voice_server_crashed -> 4015
  | `Unknown_encryption_mode -> 4016

let of_close_code_exn = function
  | #Websocket.Close_code.standard as c -> c
  | `Other 4001 -> `Unknown_op
  | `Other 4002 -> `Invalid_payload
  | `Other 4003 -> `Not_authenticated
  | `Other 4004 -> `Authentication_failed
  | `Other 4005 -> `Already_authenticated
  | `Other 4006 -> `Invalid_session
  | `Other 4009 -> `Session_timeout
  | `Other 4011 -> `Server_not_found
  | `Other 4012 -> `Unknown_protocol
  | `Other 4014 -> `Disconnected
  | `Other 4015 -> `Voice_server_crashed
  | `Other 4016 -> `Unknown_encryption_mode
  | `Other c -> failwith (Printf.sprintf "unknown voice close code: %d" c)

let pp =
  let pp' fmt = function
    | `Unknown_op -> Format.fprintf fmt "invalid opcode"
    | `Invalid_payload -> Format.fprintf fmt "invalid payload while identifying"
    | `Not_authenticated -> Format.fprintf fmt "not authenticated"
    | `Authentication_failed -> Format.fprintf fmt "authentication failed"
    | `Already_authenticated -> Format.fprintf fmt "already authenticated"
    | `Invalid_session -> Format.fprintf fmt "session is no longer valid"
    | `Session_timeout -> Format.fprintf fmt "session timed out"
    | `Server_not_found -> Format.fprintf fmt "voice server not found"
    | `Unknown_protocol -> Format.fprintf fmt "unknown protocol"
    | `Disconnected -> Format.fprintf fmt "disconnected"
    | `Voice_server_crashed -> Format.fprintf fmt "voice server crashed"
    | `Unknown_encryption_mode -> Format.fprintf fmt "unknown encryption mode"
  in
  fun fmt t ->
    match t with
    | #Websocket.Close_code.standard as c -> Websocket.Close_code.pp fmt c
    | #discord as t -> Format.fprintf fmt "(%d %a)" (to_int t) pp' t

let is_recoverable = function
  | `Unknown_op | `Invalid_payload | `Not_authenticated | `Authentication_failed
  | `Already_authenticated | `Unknown_protocol | `Disconnected
  | `Unknown_encryption_mode
  | #Websocket.Close_code.standard ->
      false
  | _ -> true
