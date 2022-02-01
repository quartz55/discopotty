open! Disco_core.Globals
open Lwt.Infix
module Snowflake = Disco_models.Snowflake
module E = Disco_core.Events
module Sf_map = Map.Make (Snowflake)
module Gateway = Disco_core.Gateway

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field

type t = { mutable calls : Call.t Sf_map.t; gw : Gateway.t }

let user_id { gw; _ } =
  let user = Gateway.user gw in
  user.id

let server_update t srv =
  L.info (fun m -> m "srv update");
  Sf_map.get srv.E.Voice_server_update.guild_id t.calls
  |> Option.map (fun call ->
         Call.update_server ~token:srv.token ~endpoint:srv.endpoint call)
  |> Option.get_or ~default:Lwt.return_unit

let state_update t vst =
  L.info (fun m -> m "state update");
  match vst.E.Voice_state.guild_id with
  | Some guild_id when Snowflake.(user_id t = vst.user_id) ->
      Sf_map.get guild_id t.calls
      |> Option.map (fun call ->
             Call.update_session ?channel_id:vst.channel_id
               ~session_id:vst.session_id call)
      |> Option.get_or ~default:Lwt.return_unit
  | _ -> Lwt.return_unit

let make gw =
  let t = { calls = Sf_map.empty; gw } in
  let evloop () =
    let open Lwt.Syntax in
    let* evs = Gateway.events gw |> Mpmc.Sink.clone in
    let rec fwd () =
      Mpmc.Sink.pull evs >>= function
      | Some (E.Voice_server_update v) -> server_update t v >>= fwd
      | Some (Voice_state_update v) -> state_update t v >>= fwd
      | Some _ -> fwd ()
      | None -> Lwt.return_unit
    in
    fwd ()
  in
  Lwt.async evloop;
  t

let destroy t =
  Sf_map.to_seq t.calls
  |> Seq.map (fun (_, call) -> Call.leave call)
  |> Seq.to_list |> Lwt.join
  >|= fun () -> t.calls <- Sf_map.empty

let get t ~guild_id =
  match Sf_map.get guild_id t.calls with
  | Some call -> call
  | None ->
      let call = Call.make t.gw ~guild_id |> Result.get_exn in
      t.calls <- Sf_map.add guild_id call t.calls;
      call