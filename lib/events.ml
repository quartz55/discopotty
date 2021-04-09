open Globals

module Ready = struct
  type t = {
    v : int;
    user : Models.User.t;
    guilds : Models.Guild.unavailable list;
    session_id : string;
    shard : (int * int) option; [@yojson.option]
    application : Models.Application.partial;
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Voice_state = struct
  type t = {
    guild_id : Models.Snowflake.t option; [@yojson.option]
    channel_id : Models.Snowflake.t option;
    user_id : Models.Snowflake.t;
    member : Models.Guild_member.t option; [@yojson.option]
    session_id : string;
    deaf : bool;
    mute : bool;
    self_deaf : bool;
    self_mute : bool;
    self_stream : bool option; [@yojson.option]
    self_video : bool;
    suppress : bool;
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Voice_server_update = struct
  type t = { token : string; guild_id : Models.Snowflake.t; endpoint : string }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Channel_pins_update = struct
  type t = {
    guild_id : Models.Snowflake.t option; [@yojson.option]
    channel_id : Models.Snowflake.t;
    last_pin_timestamp : int option option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_user = struct
  type t = { guild_id : Models.Snowflake.t; user : Models.User.t }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_emojis = struct
  type t = { guild_id : Models.Snowflake.t; emojis : Models.Emoji.t list }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_member_update = struct
  type t = {
    guild_id : Models.Snowflake.t;
    roles : Models.Snowflake.t list;
    user : Models.User.t;
    nick : string option option; [@yojson.option]
    joined_at : string;
    premium_since : string option option; [@yojson.option]
    pending : bool option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Presence = struct
  type t = {
    user : Models.User.partial;
    guild_id : Models.Snowflake.t;
    status : Models.Presence_status.unsupported;
    activities : Models.Activity.t list;
    client_status : client_status;
  }

  and client_status = {
    desktop : Models.Presence_status.unsupported option; [@yojson]
    mobile : Models.Presence_status.unsupported option; [@yojson]
    web : Models.Presence_status.unsupported option; [@yojson]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_members_chunk = struct
  type t = {
    guild_id : Models.Snowflake.t;
    members : Models.Guild_member.t list;
    chunk_index : int;
    chunk_count : int;
    not_found : Models.Snowflake.t list option; [@yojson.option]
    presences : Presence.t list option; [@yojson.option]
    nonce : string option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_role = struct
  type t = { guild_id : Models.Snowflake.t; role : Models.Role.t }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Guild_role_id = struct
  type t = { guild_id : Models.Snowflake.t; role_id : Models.Snowflake.t }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

type guild_obj = { guild_id : Models.Snowflake.t }
[@@deriving yojson, show] [@@yojson.allow_extra]

module Message_delete = struct
  type t = {
    id : Models.Snowflake.t;
    channel_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Message_delete_bulk = struct
  type t = {
    ids : Models.Snowflake.t list;
    channel_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Message_reaction_add = struct
  type t = {
    user_id : Models.Snowflake.t;
    channel_id : Models.Snowflake.t;
    message_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
    member : Models.Guild_member.t option; [@yojson.option]
    emoji : Models.Emoji.reaction;
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Message_reaction_remove = struct
  type t = {
    user_id : Models.Snowflake.t;
    channel_id : Models.Snowflake.t;
    message_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
    emoji : Models.Emoji.reaction;
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Message_reaction_remove_all = struct
  type t = {
    channel_id : Models.Snowflake.t;
    message_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Message_reaction_remove_emoji = struct
  type t = {
    channel_id : Models.Snowflake.t;
    message_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
    emoji : Models.Emoji.reaction;
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

module Typing_start = struct
  type t = {
    channel_id : Models.Snowflake.t;
    guild_id : Models.Snowflake.t option; [@yojson.option]
    user_id : Models.Snowflake.t;
    timestamp : int;
    member : Models.Guild_member.t option; [@yojson.option]
  }
  [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]
end

type t =
  (* protocol *)
  | Ready of Ready.t
  | Resumed
  (* channels *)
  | Channel_create of Models.Channel.t
  | Channel_update of Models.Channel.t
  | Channel_delete of Models.Channel.t
  | Channel_pins_update of Channel_pins_update.t
  (* guilds *)
  | Guild_create of Models.Guild.t
  | Guild_update of Models.Guild.t
  | Guild_delete of Models.Guild.unavailable
  | Guild_ban_add of Guild_user.t
  | Guild_ban_remove of Guild_user.t
  | Guild_emojis_update of Guild_emojis.t
  | Guild_integrations_update of Models.Snowflake.t
  | Guild_member_add of {
      guild_id : Models.Snowflake.t;
      member : Models.Guild_member.t;
    }
  | Guild_member_remove of Guild_user.t
  | Guild_member_update of Guild_member_update.t
  | Guild_members_chunk of Guild_members_chunk.t
  | Guild_role_create of Guild_role.t
  | Guild_role_update of Guild_role.t
  | Guild_role_delete of Guild_role_id.t
  (* TODO invites *)
  | Invite_create
  | Invite_delete
  (* messages *)
  | Message_create of Models.Message.t
  | Message_update of Models.Message.partial
  | Message_delete of Message_delete.t
  | Message_delete_bulk of Message_delete_bulk.t
  | Message_reaction_add of Message_reaction_add.t
  | Message_reaction_remove of Message_reaction_remove.t
  | Message_reaction_remove_all of Message_reaction_remove_all.t
  | Message_reaction_remove_emoji of Message_reaction_remove_emoji.t
  (* users *)
  | Presence_update of Presence.t
  | Typing_start of Typing_start.t
  | User_update of Models.User.t
  (* voice *)
  | Voice_state_update of Voice_state.t
  | Voice_server_update of Voice_server_update.t
  (* TODO webhooks *)
  | Webhooks_update
  (* TODO commands *)
  | Application_command_create
  | Application_command_update
  | Application_command_delete
  (* TODO interactions *)
  | Interaction_create
  | Unsupported of string * Yojson.Safe.t option
[@@deriving show]

let of_name name data =
  match (name, data) with
  (* protocol *)
  | "READY", Some d -> Ready (Ready.t_of_yojson d)
  | "RESUMED", _ -> Resumed
  (* channels *)
  | "CHANNEL_CREATE", Some d -> Channel_create (Models.Channel.t_of_yojson d)
  | "CHANNEL_UPDATE", Some d -> Channel_update (Models.Channel.t_of_yojson d)
  | "CHANNEL_DELETE", Some d -> Channel_delete (Models.Channel.t_of_yojson d)
  | "CHANNEL_PINS_UPDATE", Some d ->
      Channel_pins_update (Channel_pins_update.t_of_yojson d)
  (* guilds *)
  | "GUILD_CREATE", Some d -> Guild_create (Models.Guild.t_of_yojson d)
  | "GUILD_UPDATE", Some d -> Guild_update (Models.Guild.t_of_yojson d)
  | "GUILD_DELETE", Some d ->
      Guild_delete (Models.Guild.unavailable_of_yojson d)
  | "GUILD_BAN_ADD", Some d -> Guild_ban_add (Guild_user.t_of_yojson d)
  | "GUILD_BAN_REMOVE", Some d -> Guild_ban_remove (Guild_user.t_of_yojson d)
  | "GUILD_EMOJIS_UPDATE", Some d ->
      Guild_emojis_update (Guild_emojis.t_of_yojson d)
  | "GUILD_INTEGRATIONS_UPDATE", Some d ->
      let { guild_id } = guild_obj_of_yojson d in
      Guild_integrations_update guild_id
  | "GUILD_MEMBER_ADD", Some d ->
      let { guild_id } = guild_obj_of_yojson d in
      Guild_member_add { guild_id; member = Models.Guild_member.t_of_yojson d }
  | "GUILD_MEMBER_REMOVE", Some d ->
      Guild_member_remove (Guild_user.t_of_yojson d)
  | "GUILD_MEMBER_UPDATE", Some d ->
      Guild_member_update (Guild_member_update.t_of_yojson d)
  | "GUILD_MEMBERS_CHUNK", Some d ->
      Guild_members_chunk (Guild_members_chunk.t_of_yojson d)
  | "GUILD_ROLE_CREATE", Some d -> Guild_role_create (Guild_role.t_of_yojson d)
  | "GUILD_ROLE_UPDATE", Some d -> Guild_role_update (Guild_role.t_of_yojson d)
  | "GUILD_ROLE_DELETE", Some d ->
      Guild_role_delete (Guild_role_id.t_of_yojson d)
  (* TODO invites *)
  | "INVITE_CREATE", _ -> Invite_create
  | "INVITE_DELETE", _ -> Invite_delete
  (* messages *)
  | "MESSAGE_CREATE", Some d -> Message_create (Models.Message.t_of_yojson d)
  | "MESSAGE_UPDATE", Some d ->
      Message_update (Models.Message.partial_of_yojson d)
  | "MESSAGE_DELETE", Some d -> Message_delete (Message_delete.t_of_yojson d)
  | "MESSAGE_DELETE_BULK", Some d ->
      Message_delete_bulk (Message_delete_bulk.t_of_yojson d)
  | "MESSAGE_CREATION_ADD", Some d ->
      Message_reaction_add (Message_reaction_add.t_of_yojson d)
  | "MESSAGE_CREATION_REMOVE", Some d ->
      Message_reaction_remove (Message_reaction_remove.t_of_yojson d)
  | "MESSAGE_CREATION_REMOVE_ALL", Some d ->
      Message_reaction_remove_all (Message_reaction_remove_all.t_of_yojson d)
  | "MESSAGE_CREATION_REMOVE_EMOJI", Some d ->
      Message_reaction_remove_emoji
        (Message_reaction_remove_emoji.t_of_yojson d)
  (* users *)
  | "PRESENCE_UPDATE", Some d -> Presence_update (Presence.t_of_yojson d)
  | "TYPING_START", Some d -> Typing_start (Typing_start.t_of_yojson d)
  | "USER_UPDATE", Some d -> User_update (Models.User.t_of_yojson d)
  (* voice *)
  | "VOICE_STATE_UPDATE", Some d ->
      Voice_state_update (Voice_state.t_of_yojson d)
  | "VOICE_SERVER_UPDATE", Some d ->
      Voice_server_update (Voice_server_update.t_of_yojson d)
  (* TODO webhooks *)
  | "WEBHOOKS_UPDATE", _ -> Webhooks_update
  (* TODO commands *)
  | "APPLICATION_COMMAND_CREATE", _ -> Application_command_create
  | "APPLICATION_COMMAND_UPDATE", _ -> Application_command_update
  | "APPLICATION_COMMAND_DELETE", _ -> Application_command_delete
  (* TODO interactions *)
  | "INTERACTION_CREATE", _ -> Interaction_create
  | other, d -> Unsupported (other, d)
