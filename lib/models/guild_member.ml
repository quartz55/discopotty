type t = {
  user : User.t option; [@yojson.option]
  nick : string option option; [@yojson.option]
  roles : Snowflake.t list;
  joined_at : string;
  premium_since : string option option; [@yojson.option]
  deaf : bool;
  mute : bool;
  pending : bool option; [@yojson.option]
  permissions : string option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
