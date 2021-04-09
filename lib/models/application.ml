type partial = { id : Snowflake.t; flags : int }
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
