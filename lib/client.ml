open Disco_core
open! Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module SfMap = Map.Make (Models.Snowflake)

module type Voice_manager_intf = sig
  type t

  val make : Gateway.t -> t

  val destroy : t -> unit Lwt.t
end

module Make (Voice : Voice_manager_intf) = struct
  type t = { http : Http.t; gw : Gateway.t; cache : Cache.t; voice : Voice.t }

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

  let gateway { gw; _ } = gw

  let disconnect { gw; voice; _ } =
    Voice.destroy voice >>= fun () -> Gateway.disconnect gw

  let voice { voice; _ } = voice

  let run ~handler token =
    let open Lwt_result.Syntax in
    L.info (fun m -> m "creating HTTP client");
    let* http = Http.create token in
    L.debug (fun m -> m "getting gateway url");
    let* gw_info = Http.get_gateway_info http |> Lwt_result.ok in
    let url, max_concurrency =
      match gw_info with
      | Fallback { url; max_concurrency } -> (url, max_concurrency)
      | Real { url; session_start_limit; _ } ->
          (url, session_start_limit.max_concurrency)
    in
    L.info (fun m -> m "connecting to gateway");
    let* gw = Gateway.connect ~max_concurrency ~token url in
    L.info (fun m -> m "initialising cache");
    let* cache = Cache.create () |> Lwt_result.ok in
    L.info (fun m -> m "initialising voice manager");
    let voice = Voice.make gw in
    let t = { http; gw; cache; voice } in
    L.info (fun m -> m "starting main event loop");
    let ev_handler = handler t in
    let evs = Gateway.events gw in
    let rec fwd () =
      Mpmc.Sink.pull evs >>= function
      | Some ev -> ev_handler ev >>= fwd
      | None -> Lwt.return_ok ()
    in
    fwd ()
end

module Nop_voice = struct
  type t = Nop

  let make _ = Nop

  let destroy _ = Lwt.return_unit
end

include Make (Nop_voice)
