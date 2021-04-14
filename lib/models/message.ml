module Channel_mention = struct
  type t = {
    id : Snowflake.t;
    guild_id : Snowflake.t;
    typ : int; [@key "type"]
    name : string;
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]
end

type t = {
  id : Snowflake.t;
  channel_id : Snowflake.t;
  guild_id : Snowflake.t option; [@yojson.option]
  author : User.t;
  member : Guild_member.t option; [@yojson.option]
  content : string;
  timestamp : string;
  edited_timestamp : string option;
  tts : bool;
  mention_everyone : bool;
  mentions : User.t list;
  mention_roles : Snowflake.t list;
  mention_channels : Channel_mention.t list option; [@yojson.option]
  (* attachments: Attachment.t list; *)
  (* embeds: Embed.t list; *)
  (* reactions: Reaction.t list option; [@yojson.option] *)
  nonce : string option; [@yojson.option]
  pinned : bool;
  webhook_id : Snowflake.t option; [@yojson.option]
  typ : int; [@key "type"]
  (* activity : Activity.t option; [@yojson.option] *)
  (* application : Application.t option; [@yojson.option] *)
  (* message_reference : Reference.t option; [@yojson.option] *)
  flags : int option; [@yojson.option]
  (* stickers : Sticker.t list option; [@yojson.option] *)
  referenced_message : t option option; [@yojson.option]
      (* interaction : Interaction.t option; [@yojson.option] *)
}
[@@deriving yojson, show] [@@yojson.allow_extra_fields]

type partial = {
  id : Snowflake.t;
  channel_id : Snowflake.t;
  content : string option; [@yojson.option]
  timestamp : string option; [@yojson.option]
  edited_timestamp : string option; [@yojson.option]
}
[@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

module Utils = struct
  let wrap ~c s =
    let c_len = String.length c in
    let len = String.length s in
    let buf = Bytes.create (len + (2 * c_len)) in
    Bytes.blit_string c 0 buf 0 c_len;
    Bytes.blit_string s 0 buf c_len len;
    Bytes.blit_string c 0 buf (len + c_len) c_len;
    Bytes.unsafe_to_string buf

  let ( & ) x y a = x a |> y

  let ( || ) f a = f a

  let bold = wrap ~c:"**"

  let b = bold

  let strong = bold

  let italic = wrap ~c:"_"

  let i = italic

  let it = italic

  let verbatim = wrap ~c:"`"

  let v = verbatim

  let code = verbatim

  let _setup_ppf =
    let wrap : or_else:(Format.stag -> string) -> Format.stag -> string =
     fun ~or_else -> function
      | Format.String_tag s -> (
          match String.trim s with
          | "bold" | "b" | "strong" -> "**"
          | "italic" | "i" | "it" -> "_"
          | "verbatim" | "v" | "code" | "~" -> "`"
          | _ -> "")
      | t -> or_else t
    in
    let k ppf =
      let fns = Format.pp_get_formatter_stag_functions ppf () in
      let mark_tags = Format.pp_get_mark_tags ppf () in
      Format.pp_set_tags ppf true;
      Format.pp_set_mark_tags ppf true;
      Format.pp_set_formatter_stag_functions ppf
        {
          fns with
          mark_open_stag = wrap ~or_else:fns.mark_open_stag;
          mark_close_stag = wrap ~or_else:fns.mark_close_stag;
        };
      fun () ->
        Format.pp_set_formatter_stag_functions ppf fns;
        Format.pp_set_mark_tags ppf mark_tags
    in
    k

  let fprintf ppf fmt =
    let restore = _setup_ppf ppf in
    Format.kfprintf (fun _ -> restore ()) ppf fmt

  let kfprintf ~k ppf fmt =
    let restore = _setup_ppf ppf in
    Format.kfprintf
      (fun ppf ->
        let out = k ppf in
        restore ();
        out)
      ppf fmt
end

let fmt fmt =
  let buf = Buffer.create 64 in
  Utils.kfprintf
    ~k:(fun ppf ->
      Format.pp_print_flush ppf ();
      Buffer.contents buf)
    (Format.formatter_of_buffer buf)
    fmt
