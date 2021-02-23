type t = {
  id : Snowflake.t;
  channel_id : Snowflake.t;
  guild_id : Snowflake.t option;
  content : string;
  type_ : int; [@key "type"]
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

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
