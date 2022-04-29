open Disco_core
open! Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module SfMap = Map.Make (Models.Snowflake)

module type Voice_manager_intf = sig
  type t

  val make : env:Eio.Stdenv.t -> sw:Eio.Switch.t -> Gateway.t -> t
  val destroy : t -> unit
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
    match Http.post ~body:ser uri http with
    | Ok _ -> ()
    | Error _ -> L.error (fun m -> m "error sending message")

  let gateway { gw; _ } = gw

  let disconnect { gw; voice; _ } =
    Voice.destroy voice;
    Gateway.disconnect gw

  let voice { voice; _ } = voice

  let run ~env ~handler token =
    let net = Eio.Stdenv.net env in
    let dmgr = Eio.Stdenv.domain_mgr env in
    Switch.run @@ fun sw ->
    L.info (fun m -> m "creating HTTP client");
    let http = Http.create ~sw ~net token in
    L.debug (fun m -> m "getting gateway url");
    let gw_info = Http.get_gateway_info http in
    let url, max_concurrency =
      match gw_info with
      | Fallback { url; max_concurrency } -> (url, max_concurrency)
      | Real { url; session_start_limit; _ } ->
          (url, session_start_limit.max_concurrency)
    in
    L.info (fun m -> m "connecting to gateway");
    let gw = Gateway.connect ~sw ~net ~dmgr ~max_concurrency ~token url in
    L.info (fun m -> m "initialising cache");
    let cache = Cache.create () in
    L.info (fun m -> m "initialising voice manager");
    let voice = Voice.make ~env ~sw gw in
    let t = { http; gw; cache; voice } in
    L.info (fun m -> m "starting main event loop");
    let fn =
      let h = handler t in
      fun ev ->
        try h ev
        with e -> (
          match ev with
          | Events.Message_create { channel_id; _ } ->
              let msg =
                Models.Message.fmt "unhandled client handler exception: %s"
                  (Printexc.to_string e)
              in
              send_message channel_id msg t
          | _ ->
              L.warn (fun m ->
                  m "unhandled client handler exception: %s"
                    (Printexc.to_string e)))
    in
    let _unsub = Gateway.sub ~fn gw in
    ()
end

module Nop_voice = struct
  type t = Nop

  let make ~env:_ ~sw:_ _ = Nop
  let destroy _ = ()
end

include Make (Nop_voice)
