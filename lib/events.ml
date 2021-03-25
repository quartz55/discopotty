open Globals

module Ready = struct
  type t = {
    v : int;
    user : Models.User.t;
    (* guilds : Models.Guild.t list; *)
    session_id : string;
    shard : (int * int) option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module VoiceState = struct
  type t = {
    guild_id : Models.Snowflake.t option; [@yojson.option]
    channel_id : Models.Snowflake.t option;
    user_id : Models.Snowflake.t;
    (* member: guild option; [@yojson.option]; *)
    session_id : string;
    deaf : bool;
    mute : bool;
    self_deaf : bool;
    self_mute : bool;
    self_stream : bool option; [@yojson.option]
    self_video : bool;
    suppress : bool;
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module VoiceServerUpdate = struct
  type t = { token : string; guild_id : Models.Snowflake.t; endpoint : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

type t =
  | Ready of Ready.t
  | MessageCreate of Models.Message.t
  | Resumed
  | VoiceStateUpdate of VoiceState.t
  | VoiceServerUpdate of VoiceServerUpdate.t
  | Unsupported of string * Yojson.Safe.t option

let of_name name data =
  match (name, data) with
  | "READY", Some d -> Ready (Ready.t_of_yojson d)
  | "MESSAGE_CREATE", Some d -> MessageCreate (Models.Message.t_of_yojson d)
  | "RESUMED", _ -> Resumed
  | "VOICE_STATE_UPDATE", Some d -> VoiceStateUpdate (VoiceState.t_of_yojson d)
  | "VOICE_SERVER_UPDATE", Some d ->
      VoiceServerUpdate (VoiceServerUpdate.t_of_yojson d)
  | other, d -> Unsupported (other, d)
