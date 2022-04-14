type t = [ `Discord of string | `Exn of exn | `Msg of string ]
type nonrec 'a result = ('a, t) result

exception Discord of string

let of_msg s = `Msg s
let of_msgf fmt = Format.kasprintf of_msg fmt
let of_exn e = `Exn e
let of_discord s = `Discord s
let msgf fmt = Format.kasprintf (fun v -> Error (of_msg v)) fmt
let exn e = Error (of_exn e)
let msg s = Error (of_msg s)
let discord s = Error (of_discord s)

let to_string = function
  | `Discord m -> "DiscordError: " ^ m
  | `Exn exn -> Printexc.to_string exn
  | `Msg m -> m

let raise = function
  | `Discord m -> raise @@ Discord m
  | `Exn e -> raise e
  | `Message m -> failwith m

let pp fmt t = Format.fprintf fmt "%s" (to_string t)

let tap ~f r = function
  | Ok o -> Ok o
  | Error e ->
      f e;
      r
