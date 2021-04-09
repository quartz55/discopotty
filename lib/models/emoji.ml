type t = {
  id : Snowflake.t option;
  name : string;
  roles : Snowflake.t list option; [@yojson.option]
  user : User.t option; [@yojson.option]
  require_colons : bool option; [@yojson.option]
  managed : bool option; [@yojson.option]
  animated : bool option; [@yojson.option]
  available : bool option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]

type reaction = {
  id : Snowflake.t option;
  name : string option;
  animated : bool option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
