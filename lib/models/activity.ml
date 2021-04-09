type t = {
  name : string;
  typ : int; [@key "type"]
  url : string option option; [@yojson.option]
  created_at : int;
  timestamps : timestamps option; [@yojson.option]
  application_id : Snowflake.t option; [@yojson.option]
  details : string option option; [@yojson.option]
  state : string option option; [@yojson.option]
  emoji : Emoji.t option option; [@yojson.option]
  (* party : Party.t option; [@yojson.option] *)
  (* assets : Assets.t option; [@yojson.option] *)
  (* secrets : Secrets.t option; [@yojson.option] *)
  instance : bool option; [@yojson.option]
  flags : int option; [@yojson.option]
}
[@@yojson.allow_extra_fields]

and timestamps = { start : int; end_ : int [@key "end"] }
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
