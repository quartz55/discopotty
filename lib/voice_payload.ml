open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Dir = Payload.Dir
module Raw = Payload.Raw

exception Invalid_payload of Raw.t * string

let invalid_payload raw string = raise (Invalid_payload (raw, string))

module Hello = struct
  let t_of_yojson json =
    Yojson.Safe.Util.(
      json |> member "heartbeat_interval" |> to_float |> Int.of_float)
end

module Identify = struct
  type t = {
    server_id : Models.Snowflake.t;
    user_id : Models.Snowflake.t;
    session_id : string;
    token : string;
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  let make ~server_id ~user_id ~session_id ~token =
    { server_id; user_id; session_id; token }
end

module Ready = struct
  type t = { ssrc : int; ip : string; port : int; modes : string list }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]
end

module Select_protocol = struct
  type t = { protocol : string; data : data }

  and data = { address : string; port : int; mode : string }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  let make ~address ~port ~mode =
    { protocol = "udp"; data = { address; port; mode } }
end

module Session_description = struct
  type t = { mode : string; secret_key : int list }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]
end

module Resume = struct
  type t = {
    server_id : Models.Snowflake.t;
    session_id : string;
    token : string;
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  let make ~server_id ~session_id ~token = { server_id; session_id; token }
end

module Speaking = struct
  type t = {
    speaking : int;
    delay : int option; [@yojson.option]
    ssrc : int option; [@yojson.option]
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  let make ?delay ?ssrc speaking = { speaking; delay; ssrc }
end

type _ t =
  | Identify : Identify.t -> Dir.send t
  | Select_protocol : Select_protocol.t -> Dir.send t
  | Ready : Ready.t -> Dir.recv t
  | Heartbeat : int -> Dir.send t
  | Session_description : Session_description.t -> Dir.recv t
  | Speaking : Speaking.t -> _ Dir.bidi t
  | Heartbeat_ack : int -> Dir.recv t
  | Resume : Resume.t -> Dir.send t
  | Hello : int -> Dir.recv t
  | Resumed : Dir.recv t
  | Client_disconnect : Dir.recv t

include Payload.Make (struct
  type nonrec 'a t = 'a t

  let op : type a. a t -> int =
   fun t ->
    match t with
    | Identify _ -> 0
    | Select_protocol _ -> 1
    | Ready _ -> 2
    | Heartbeat _ -> 3
    | Session_description _ -> 4
    | Speaking _ -> 5
    | Heartbeat_ack _ -> 6
    | Resume _ -> 7
    | Hello _ -> 8
    | Resumed -> 9
    | Client_disconnect -> 13

  let of_raw raw =
    match (raw.Raw.op, raw.d) with
    | 2, Some d -> Ready (Ready.t_of_yojson d)
    | 4, Some d -> Session_description (Session_description.t_of_yojson d)
    | 5, Some d -> Speaking (Speaking.t_of_yojson d)
    | 6, Some d -> Heartbeat_ack ([%of_yojson: int] d)
    | 8, Some d -> Hello (Hello.t_of_yojson d)
    | 9, _ -> Resumed
    | 13, Some _d -> Client_disconnect
    | n, _ when n <= 13 ->
        invalid_payload raw
          (Format.asprintf "payload with opcode=%d is not recv type" n)
    | _ -> invalid_payload raw "unrecognized opcode"

  let to_raw t =
    op t
    |>
    match t with
    | Identify id -> Raw.make ~d:(Identify.yojson_of_t id) ()
    | Select_protocol sp -> Raw.make ~d:(Select_protocol.yojson_of_t sp) ()
    | Heartbeat nonce -> Raw.make ~d:(`Int nonce) ()
    | Speaking d -> Raw.make ~d:(Speaking.yojson_of_t d) ()
    | Resume d -> Raw.make ~d:(Resume.yojson_of_t d) ()
end)

let heartbeat nonce = Heartbeat nonce

let make_identify ~server_id ~user_id ~session_id ~token =
  Identify (Identify.make ~server_id ~user_id ~session_id ~token)

let make_select_protocol ~address ~port ~mode =
  Select_protocol (Select_protocol.make ~address ~port ~mode)

let make_resume ~server_id ~session_id ~token =
  Resume (Resume.make ~server_id ~session_id ~token)

let make_speaking ?ssrc ?delay s = Speaking (Speaking.make ?ssrc ?delay s)
