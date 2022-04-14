open Containers

exception Invalid_config of string

type t = { token : string; prefix : string }

let token { token; _ } = token
let prefix { prefix; _ } = prefix

let of_filename filename =
  match Toml.Parser.from_filename filename with
  | `Ok tbl ->
      let token =
        match
          Toml.Lenses.(
            get tbl (key "auth" |-- table |-- key "token" |-- string))
        with
        | Some t -> t
        | None ->
            raise
            @@ Invalid_config "Invalid configuration: missing [auth.token]"
      in
      let prefix =
        Toml.Lenses.(
          get tbl (key "commands" |-- table |-- key "prefix" |-- string))
        |> Option.get_or ~default:"!"
      in
      { token; prefix }
  | `Error (_, loc) ->
      let msg =
        Format.sprintf
          "Invalid TOML in configuration: @[file='%s'@] @[line=%d@] \
           @[column=%d@]@."
          filename loc.line loc.column
      in
      raise @@ Invalid_config msg
