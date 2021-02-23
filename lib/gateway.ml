open Containers

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Payload = Gateway_payload

type t = { session : Session.t }

let get_gateway_url http =
  let api () =
    let open Lwt_result.Syntax in
    let* res = Http.get "/gateway/bot" http in
    let* body = Piaf.Body.to_string res.body in
    Result.guard_str (fun () ->
        let raw = Yojson.Safe.from_string body in
        Yojson.Safe.Util.(raw |> member "url" |> to_string))
    |> Result.map_err (fun e -> `Msg e)
    |> Lwt.return
  in
  api ()
  |> Lwt.map (function
       | Ok url -> url
       | Error e ->
           L.err (fun m ->
               m "Couldn't get gateway url from API, using fallback: %s@."
                 (Piaf.Error.to_string e));
           "wss://gateway.discord.gg")
  |> Lwt_result.ok

let connect ?http token =
  let open Lwt_result.Syntax in
  let* http =
    match http with None -> Http.create token | Some d -> Lwt_result.return d
  in
  let* url = get_gateway_url http in
  let+ session = Session.create token (Uri.of_string url) in
  { session }

let disconnect { session; _ } = Session.disconnect session

let events { session; _} = Session.events session