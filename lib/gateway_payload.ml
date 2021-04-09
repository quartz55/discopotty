open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Intents = struct
  type t = int [@@deriving yojson]

  let make = List.fold_left (fun out intent -> out lor intent) 0

  let add i t = t lor i

  let rm i t = lnot i land t

  let guilds = 1 lsl 0

  let guild_members = 1 lsl 1

  let guild_bans = 1 lsl 2

  let guild_emojis = 1 lsl 3

  let guild_integrations = 1 lsl 4

  let guild_webhooks = 1 lsl 5

  let guild_invites = 1 lsl 6

  let guild_voice_states = 1 lsl 7

  let guild_presences = 1 lsl 8

  let guild_messages = 1 lsl 9

  let guild_message_reactions = 1 lsl 10

  let guild_message_typing = 1 lsl 11

  let direct_messages = 1 lsl 12

  let direct_message_reactions = 1 lsl 13

  let direct_message_typing = 1 lsl 14

  let all = (1 lsl 15) - 1

  let all_unprivileged =
    List.fold_left (fun is i -> rm i is) all [ guild_presences; guild_members ]

  let pp fmt t =
    let out =
      let rec aux acc d miss =
        match (d, miss) with
        | 0, 0 -> String.concat "" acc
        | 0, n when n > 0 -> String.make n '0' ^ String.concat "" acc
        | _, n -> aux (string_of_int (d land 1) :: acc) (d lsr 1) (n - 1)
      in
      aux [] t 15
    in
    Format.fprintf fmt "%s" out

  let show = Format.asprintf "%a" pp
end

module Identify = struct
  type t = {
    token : string;
    properties : conn_props;
    compress : bool option; [@yojson.option]
    large_threshold : int option; [@yojson.option]
    shard : (int * int) option; [@yojson.option]
    guild_subscriptions : bool option; [@yojson.option]
    intents : Intents.t;
  }

  and conn_props = {
    os : string; [@key "$os"]
    browser : string; [@key "$browser"]
    device : string; [@key "$device"]
  }
  [@@deriving yojson_of, show]

  let op = 2

  let make_props ?os ?browser ?device () =
    let get_os () =
      match Sys.os_type with
      | "Unix" -> (
          try
            let ic = Unix.open_process_in "uname" in
            let out = input_line ic in
            close_in ic;
            out |> String.lowercase_ascii
          with _ -> "unix")
      | "Win32" | "Cygwin" -> "windows"
      | _ -> assert false
    in
    let os = os |> Option.get_lazy get_os in
    let browser = browser |> Option.get_or ~default:"discopotty" in
    let device = device |> Option.get_or ~default:"discopotty" in
    { os; browser; device }

  let make ?compress ?large_threshold ?shard ?guild_subscriptions
      ?(properties = make_props ()) ?(intents = Intents.all_unprivileged) token
      =
    {
      token;
      intents;
      properties;
      compress;
      large_threshold;
      shard;
      guild_subscriptions;
    }
end

module Resume = struct
  type t = { token : string; session_id : string; seq : int }
  [@@deriving yojson_of, show]

  let op = 6
end

module Presence = struct
  type t = {
    since : int option; [@yojson.option]
    activities : Models.Activity.t list;
    status : Models.Presence_status.t;
    afk : bool;
  }
  [@@deriving yojson_of, show]

  let make ?since ?(activities = []) ~afk status =
    { since; activities; status; afk }
end

module Voice_state = struct
  type t = {
    guild_id : Models.Snowflake.t;
    channel_id : Models.Snowflake.t option;
    self_mute : bool;
    self_deaf : bool;
  }
  [@@deriving yojson_of, show]

  let make ?channel_id ~self_mute ~self_deaf guild_id =
    { guild_id; channel_id; self_mute; self_deaf }
end

module Guild_request_members = struct
  type t = {
    guild_id : Models.Snowflake.t;
    query : string option; [@yojson.option]
    limit : int;
    presences : bool option; [@yojson.option]
    user_ids : Models.Snowflake.t list option; [@yojson.option]
    nonce : string option; [@yojson.option]
  }
  [@@deriving yojson_of, show]

  type q =
    [ `All
    | `User of Models.Snowflake.t
    | `Users of Models.Snowflake.t list
    | `Query of string
    | `QueryLimit of string * int ]

  let make ?presences ?nonce ~q guild_id =
    let query, limit, user_ids =
      match q with
      | `All -> (Some "", 0, None)
      | `User id -> (None, 0, Some [ id ])
      | `Users users -> (None, 0, Some users)
      | `QueryLimit (q, l) -> (Some q, l, None)
      | `Query q -> (Some q, 0, None)
    in
    { guild_id; query; limit; presences; user_ids; nonce }
end

module Dir = Payload.Dir
module Raw = Payload.Raw

exception Invalid_payload of Raw.t * string

let invalid_payload raw string = raise (Invalid_payload (raw, string))

type _ t =
  | Dispatch : int * Events.t -> Dir.recv t
  | Heartbeat : _ Dir.bidi t
  | Identify : Identify.t -> Dir.send t
  | Presence_update : Presence.t -> Dir.send t
  | Voice_state_update : Voice_state.t -> Dir.send t
  | Resume : Resume.t -> Dir.send t
  | Reconnect : Dir.recv t
  | Request_guild_members : Guild_request_members.t -> Dir.send t
  | Invalid_session : bool -> Dir.recv t
  | Hello : int -> Dir.recv t
  | Heartbeat_ack : Dir.recv t

include Payload.Make (struct
  type nonrec 'a t = 'a t

  let op : type a. a t -> int =
   fun t ->
    match t with
    | Dispatch _ -> 0
    | Heartbeat -> 1
    | Identify _ -> 2
    | Presence_update _ -> 3
    | Voice_state_update _ -> 4
    | Resume _ -> 6
    | Reconnect -> 7
    | Request_guild_members _ -> 8
    | Invalid_session _ -> 9
    | Hello _ -> 10
    | Heartbeat_ack -> 11

  let of_raw raw =
    match (raw.Raw.op, raw.d) with
    | n, _ when n < 0 -> invalid_payload raw "negative opcode"
    | 0, d ->
        let s, t =
          match (raw.s, raw.t) with
          | Some (Some s), Some (Some t) -> (s, t)
          | _ -> invalid_payload raw "Dispatch missing 's' and/or 't'"
        in
        Dispatch (s, Events.of_name t d)
    | 1, _ -> Heartbeat
    | 7, _ -> Reconnect
    | 9, Some d -> Invalid_session ([%of_yojson: bool] d)
    | 9, None ->
        L.warn (fun m ->
            m
              "got Invalid_session payload without 'resumable' boolean, \
               defaulting to false");
        Invalid_session false
    | 10, Some d ->
        let hb =
          Yojson.Safe.Util.(d |> member "heartbeat_interval" |> to_int)
        in
        Hello hb
    | 10, None -> invalid_payload raw "Hello missing 'heartbeat_interval'"
    | 11, _ -> Heartbeat_ack
    | n, _ when n <= 11 ->
        invalid_payload raw
          (Format.asprintf "payload with opcode=%d is not recv type" n)
    | _ -> invalid_payload raw "unrecognized opcode"

  let to_raw t =
    op t
    |>
    match t with
    | Heartbeat -> Raw.make ()
    | Identify d -> Raw.make ~d:(Identify.yojson_of_t d) ()
    | Presence_update d -> Raw.make ~d:(Presence.yojson_of_t d) ()
    | Voice_state_update d -> Raw.make ~d:(Voice_state.yojson_of_t d) ()
    | Resume d -> Raw.make ~d:(Resume.yojson_of_t d) ()
    | Request_guild_members d ->
        Raw.make ~d:(Guild_request_members.yojson_of_t d) ()
end)

let heartbeat : _ t = Heartbeat

let make_identify ?compress ?large_threshold ?shard ?guild_subscriptions
    ?properties ?intents token =
  Identify
    (Identify.make ?compress ?large_threshold ?shard ?guild_subscriptions
       ?properties ?intents token)

let make_presence_update ?since ~afk status =
  Presence_update (Presence.make ?since ~afk status)

let make_voice_state_update ?channel_id ~self_mute ~self_deaf guild_id =
  Voice_state_update
    (Voice_state.make ?channel_id ~self_mute ~self_deaf guild_id)

let make_resume ~token ~session_id ~seq = Resume { token; session_id; seq }

let make_request_guild_members ?presences ?nonce ~q guild_id =
  Request_guild_members
    (Guild_request_members.make ?presences ?nonce ~q guild_id)
