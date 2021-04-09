type t = [ `online | `dnd | `idle | `invisible | `offline ] [@@deriving show]

type unsupported = [ t | `unsupported of string ] [@@deriving show]

let yojson_of_t = function
  | `online -> `String "online"
  | `dnd -> `String "dnd"
  | `idle -> `String "idle"
  | `invisible -> `String "invisible"
  | `offline -> `String "offline"

let unsupported_of_yojson : Yojson.Safe.t -> unsupported = function
  | `String "online" -> `online
  | `String "dnd" -> `dnd
  | `String "idle" -> `idle
  | `String "invisible" -> `invisible
  | `String "offline" -> `offline
  | `String other -> `unsupported other
  | _ -> invalid_arg "presence status not a string"
