module R = struct
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
end

(* module S = struct
     type voice = { user_limit : int; bitrate : int; rtc_region : string option }
     [@@deriving yojson, show] [@@yojson.allow_extra_fields]

     type guild = {
       guild_id : Snowflake.t option; [@yojson.option]
       position : int;
       permission_overwrites : R.overwrite list;
       name : string;
       topic : string option;
       nsfw : bool;
       last_message_id : Snowflake.t option;
       last_pin_timestamp : string option;
       rate_limit_per_user : int option; [@yojson.option]
       parent_id : Snowflake.t option;
     }
     [@@deriving yojson, show] [@@yojson.allow_extra_fields]
   end

   module V = struct
     let user_limit t = t.user_limit

     let bitrate t = t.bitrate

     let rtc_region t = t.rtc_region
   end

   module G = struct end

   type t = Snowflake.t * channel

   and channel =
     | Guild_text
     | DM
     | Guild_voice
     | Group_DM
     | Guild_category
     | Guild_news
     | Guild_store
     | Guild_stage_voice

   let t_of_yojson j =
     let id = Yojson.Safe.Util.(member "id" j |> Snowflake.t_of_yojson) in
     let chan =
       match Yojson.Safe.Util.(member "type" j |> to_int) with
       | 0 -> Guild_text
       | 1 -> DM
       | 2 -> Guild_voice
       | 3 -> Group_DM
       | 4 -> Guild_category
       | 5 -> Guild_news
       | 6 -> Guild_store
       | 13 -> Guild_stage_voice
       | _n -> failwith "unsupported channel type"
     in
     (id, chan) *)

type t = R.t [@@deriving yojson, show]
