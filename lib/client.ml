open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

type t = { http : Http.t; gw : Gateway.t; disconnect : unit -> unit }

type create_msg = { content : string; nonce : string; tts : bool }
[@@deriving yojson]

let send_message channel_id content { http; _ } =
  let msg = { content; nonce = Websocket.gen_nonce 20; tts = false } in
  let uri =
    Format.sprintf "/channels/%s/messages"
      (Models.Snowflake.to_string channel_id)
  in
  let ser = yojson_of_create_msg msg in
  Http.post ~body:ser uri http
  |> Lwt.map (function
       | Ok _ -> ()
       | Error _ -> L.error (fun m -> m "error sending message"))

let join_voice ~guild_id ~channel_id { gw; _ } =
  Gateway.join_voice ~guild_id ~channel_id gw

let disconnect { disconnect; _ } = disconnect ()

let create ~handler token =
  let open Lwt_result.Syntax in
  L.info (fun m -> m "creating HTTP client");
  let* http = Http.create token in
  L.info (fun m -> m "connecting to gateway");
  let* gw = Gateway.connect ~http token in
  let disconnect () = Gateway.disconnect gw in
  let t = { http; gw; disconnect } in
  Gateway.events gw
  |> Lwt_pipe.Reader.iter_s ~f:(fun ev -> handler t ev)
  |> Lwt_result.ok
