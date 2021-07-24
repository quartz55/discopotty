open Containers

type t = { token : string; prefix : string }

let token { token; _ } = token

let prefix { prefix; _ } = prefix

let of_filename filename =
  match Toml.Parser.from_filename filename with
  | `Ok tbl ->
      let open Result in
      let+ token =
        Toml.Lenses.(get tbl (key "auth" |-- table |-- key "token" |-- string))
        |> Option.to_result_lazy (fun () ->
               `Msg "Invalid configuration: missing [auth.token]")
      in
      let prefix =
        Toml.Lenses.(
          get tbl (key "commands" |-- table |-- key "prefix" |-- string))
        |> Option.get_or ~default:"!"
      in
      { token; prefix }
  | `Error (_, loc) ->
      Error.msgf
        "Invalid TOML in configuration: @[file='%s'@] @[line=%d@] \
         @[column=%d@]@."
        filename loc.line loc.column
