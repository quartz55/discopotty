type t = {
  id : Snowflake.t;
  username : string;
  discriminator : string;
  avatar : string option;
  bot : bool option; [@yojson.option]
  system : bool option; [@yojson.option]
  mfa_enabled : bool option; [@yojson.option]
  locale : string option; [@yojson.option]
  verified : bool option; [@yojson.option]
  email : string option option; [@yojson.option]
  flags : int option; [@yojson.option]
  premium_type : int option; [@yojson.option]
  public_flags : int option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]

type partial = {
  id : Snowflake.t;
  username : string option; [@yojson.option]
  discriminator : string option; [@yojson.option]
  avatar : string option; [@yojson.option]
  bot : bool option; [@yojson.option]
  system : bool option; [@yojson.option]
  mfa_enabled : bool option; [@yojson.option]
  locale : string option; [@yojson.option]
  verified : bool option; [@yojson.option]
  email : string option option; [@yojson.option]
  flags : int option; [@yojson.option]
  premium_type : int option; [@yojson.option]
  public_flags : int option; [@yojson.option]
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
