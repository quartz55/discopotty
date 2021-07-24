type t =
  [ `Discord of string | `Http of Piaf.Error.t | `Exn of exn | `Msg of string ]

type nonrec 'a result = ('a, t) result

let of_http e = `Http e

let of_msg s = `Msg s

let of_msgf fmt = Format.kasprintf of_msg fmt

let of_exn e = `Exn e

let of_discord s = `Discord s

let msg s = Error (of_msg s)

let msgf fmt = Format.kasprintf (fun v -> Error (of_msg v)) fmt

let exn e = Error (of_exn e)

let discord s = Error (of_discord s)

let to_string = function
  | `Discord m -> "DiscordError: " ^ m
  | `Http piaf -> Piaf.Error.to_string piaf
  | `Exn exn -> Printexc.to_string exn
  | `Msg m -> m

let pp fmt t = Format.fprintf fmt "%s" (to_string t)

let catch_lwt p = Lwt_result.(catch p |> map_err (fun exn -> `Exn exn))

let tap ~f r = function
  | Ok o -> Ok o
  | Error e ->
      f e;
      r

let tap_lwt ~f r = function
  | Ok o -> Lwt.return (Ok o)
  | Error e -> Lwt.bind (f e) (fun () -> r)
