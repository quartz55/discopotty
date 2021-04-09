module Feature = struct
  type t =
    [ `Invite_splash
    | `Vip_regions
    | `Vanity_url
    | `Verified
    | `Partnered
    | `Community
    | `Commerce
    | `News
    | `Discoverable
    | `Featurable
    | `Animated_icon
    | `Banner
    | `Welcome_screen_enabled
    | `Member_verification_gate_enabled
    | `Preview_enabled
    | `Unknown of string ]
  [@@deriving show]

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `String "INVITE_SPLASH" -> `Invite_splash
    | `String "VIP_REGIONS" -> `Vip_regions
    | `String "VANITY_URL" -> `Vanity_url
    | `String "VERIFIED" -> `Verified
    | `String "PARTNERED" -> `Partnered
    | `String "COMMUNITY" -> `Community
    | `String "COMMERCE" -> `Commerce
    | `String "NEWS" -> `News
    | `String "DISCOVERABLE" -> `Discoverable
    | `String "FEATURABLE" -> `Featurable
    | `String "ANIMATED_ICON" -> `Animated_icon
    | `String "BANNER" -> `Banner
    | `String "WELCOME_SCREEN_ENABLED" -> `Welcome_screen_enabled
    | `String "MEMBER_VERIFICATION_GATE_ENABLED" ->
        `Member_verification_gate_enabled
    | `String "PREVIEW_ENABLED" -> `Preview_enabled
    | `String other -> `Unknown other
    | _ -> invalid_arg "expected string"
end

module Welcome_screen = struct
  module Channel = struct
    type t = {
      channel_id : Snowflake.t;
      description : string;
      emoji_id : Snowflake.t option;
      emoji_name : string option;
    }
    [@@deriving yojson, show] [@@yojson.allow_extra_fields]
  end

  type t = { description : string option; welcome_channels : channel list }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  and channel = Channel.t
end

type t = {
  id : Snowflake.t;
  name : string;
  icon : string option;
  icon_hash : string option option; [@yojson.option]
  splash : string option;
  discovery_splash : string option;
  owner_id : Snowflake.t;
  region : string;
  afk_channel_id : Snowflake.t option;
  afk_timeout : int;
  widget_enabled : bool option; [@yojson.option]
  widget_channel_id : Snowflake.t option option; [@yojson.option]
  verification_level : int;
  default_message_notifications : int;
  explicit_content_filter : int;
  roles : Role.t list;
  emojis : Emoji.t list;
  features : Feature.t list;
  mfa_level : int;
  application_id : Snowflake.t option;
  system_channel_id : Snowflake.t option;
  system_channel_flags : int;
  rules_channel_id : Snowflake.t option;
  max_presences : int option option; [@yojson.option]
  max_members : int option; [@yojson.option]
  vanity_url_code : string option;
  description : string option;
  banner : string option;
  premium_tier : int;
  premium_subscription_count : int option; [@yojson.option]
  preferred_locale : string;
  public_updates_channel_id : Snowflake.t option;
  max_video_channel_users : int option; [@yojson.option]
  approximate_member_count : int option; [@yojson.option]
  approximate_presence_count : int option; [@yojson.option]
  welcome_screen : Welcome_screen.t option; [@yojson.option]
}
[@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type unavailable = { id : Snowflake.t; unavailable : bool }
[@@deriving yojson, show] [@@yojson.allow_extra_fields]
