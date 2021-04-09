module Overwrite = struct
  type t = {
    id : Snowflake.t;
    typ : int; [@key "type"]
    allow : string;
    deny : string;
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]
end

type t = {
  id : Snowflake.t;
  typ : int; [@key "type"]
  guild_id : Snowflake.t option; [@yojson.option]
  position : int option; [@yojson.option]
  permission_overwrites : overwrite list option; [@yojson.option]
  name : string option; [@yojson.option]
  topic : string option option; [@yojson.option]
  nsfw : bool option; [@yojson.option]
  last_message_id : Snowflake.t option option; [@yojson.option]
  bitrate : int option; [@yojson.option]
  user_limit : int option; [@yojson.option]
  rate_limit_per_user : int option; [@yojson.option]
  recipients : User.t option; [@yojson.option]
  icon : string option option; [@yojson.option]
  owner_id : Snowflake.t option; [@yojson.option]
  application_id : Snowflake.t option; [@yojson.option]
  parent_id : Snowflake.t option; [@yojson.option]
  last_pin_timestamp : string option option; [@yojson.option]
}
[@@yojson.allow_extra_fields]

and overwrite = Overwrite.t [@@deriving yojson, show]
