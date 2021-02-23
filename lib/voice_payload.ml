open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Dir = Payload.Dir
module Raw = Payload.Raw

exception Invalid_payload of Raw.t * string

let invalid_payload raw string = raise (Invalid_payload (raw, string))

type _ t =
  | Identify : Dir.send t
  | SelectProtocol : Dir.send t
  | Ready : Dir.recv t
  | Heartbeat : Dir.send t
  | SessionDescription : Dir.recv t
  | Speaking : _ Dir.bidi t
  | HeartbeatACK : Dir.recv t
  | Resume : Dir.send t
  | Hello : Dir.recv t
  | Resumed : Dir.recv t
  | ClientDisconnect : Dir.recv t

include Payload.Make (struct
  type nonrec 'a t = 'a t

  let op : type a. a t -> int =
   fun t ->
    match t with
    | Identify -> 0
    | SelectProtocol -> 1
    | Ready -> 2
    | Heartbeat -> 3
    | SessionDescription -> 4
    | Speaking -> 5
    | HeartbeatACK -> 6
    | Resume -> 7
    | Hello -> 8
    | Resumed -> 9
    | ClientDisconnect -> 13

  let of_raw raw =
    match (raw.Raw.op, raw.d) with
    | 2, Some _d -> Ready
    | 3, Some _d -> SessionDescription
    | 5, Some _d -> Speaking
    | 6, Some _d -> HeartbeatACK
    | 8, Some _d -> Hello
    | 9, Some _d -> Resumed
    | 13, Some _d -> ClientDisconnect
    | n, _ when n <= 11 ->
        invalid_payload raw
          (Format.asprintf "payload with opcode=%d is not recv type" n)
    | _ -> invalid_payload raw "unrecognized opcode"

  let to_raw t =
    op t
    |>
    match t with
    | Identify -> Raw.make ()
    | SelectProtocol -> Raw.make ()
    | Heartbeat -> Raw.make ()
    | Speaking -> Raw.make ()
    | Resume -> Raw.make ()
end)
