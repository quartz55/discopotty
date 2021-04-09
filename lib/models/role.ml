type t = {
  id : Snowflake.t;
  name : string;
  color : int;
  hoist : bool;
  position : int;
  permissions : string;
  managed : bool;
  mentionable : bool;
  tags : tags option; [@yojson.option]
}

and tags = {
  bot_id : Snowflake.t option; [@yojson.option]
  interation_id : Snowflake.t option; [@yojson.option]
  premium_subscriber : unit option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
