open! Disco_core.Globals
open Lwt.Infix
module Snowflake = Disco_models.Snowflake
module E = Disco_core.Events
module Sf_map = Map.Make (Snowflake)
module Gateway = Disco_core.Gateway

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field

module Call = struct
  (* TODO this should really be /yet another/ event loop *)
  module L = (val Relog.logger ~namespace:"Call" ())

  type t = {
    gw : Gateway.t;
    guild_id : snowflake;
    mutable muted : bool;
    mutable deafened : bool;
    mutable conn : conn;
    mutable req : (unit, Error.t) result Lwt.u option;
  }

  and conn =
    | Init of snowflake
    | Server of { channel_id : snowflake; token : string; endpoint : string }
    | Session of { channel_id : snowflake; session_id : string }
    | Live of { mixer : Mixer.t; session : Session.t }
    | Detached

  let make ?(muted = false) ?(deafened = false) ~gw guild_id =
    { gw; guild_id; muted; deafened; conn = Detached; req = None }

  let guild_id { guild_id; _ } = guild_id

  let muted { muted; _ } = muted

  let deafened { deafened; _ } = deafened

  let is_live { conn; _ } = match conn with Live _ -> true | _ -> false

  let session { conn; _ } =
    match conn with
    | Live { mixer; session } -> Some (mixer, session)
    | _ -> None

  let session_exn t = session t |> Option.get_exn

  let notify_req t res =
    match (res, t.req) with
    | Ok _, Some req ->
        t.req <- None;
        Lwt.wakeup_later req (Ok ())
    | (Error _ as err), Some req ->
        t.conn <- Detached;
        t.req <- None;
        Lwt.wakeup_later req err
    | _ -> ()

  let connect t ~channel_id ~session_id ~token ~endpoint =
    let inner () =
      let open Lwt_result.Syntax in
      let user_id = (Gateway.user t.gw).id in
      let guild_id = t.guild_id in
      let* session =
        Session.create ~guild_id ~user_id ~channel_id ~session_id ~token
          endpoint
      in
      let+ mixer = Mixer.create session |> Lwt_result.lift in
      t.conn <- Live { mixer; session };
      (mixer, session)
    in
    inner () >|= notify_req t

  let should_reconnect s = function
    | `Server (token, endpoint) ->
        String.(Session.token s <> token || Session.endpoint s <> endpoint)
    | `Session (session_id, channel_id) ->
        String.(Session.session_id s <> session_id)
        || Snowflake.(Session.channel_id s <> channel_id)

  let update t upd =
    match (t.conn, upd) with
    | Init channel_id, `Server (token, endpoint) ->
        t.conn <- Server { channel_id; token; endpoint };
        Lwt.return_unit
    | (Init _ | Session _ | Server _), `Session (_, None) ->
        L.err (fun m ->
            m "got voice state update with no channel while joining?!?!");
        failwith "unimplemented"
    | Init cid, `Session (session_id, Some channel_id) ->
        if Snowflake.(cid <> channel_id) then
          L.warn (fun m ->
              m "got unexpected channel_id while joining"
                ~fields:
                  F.
                    [
                      str "got" (Snowflake.to_string channel_id);
                      str "expected" (Snowflake.to_string cid);
                    ]);
        t.conn <- Session { channel_id; session_id };
        Lwt.return_unit
    | Server s, `Server (token, endpoint) ->
        t.conn <- Server { s with token; endpoint };
        Lwt.return_unit
    | Session _, `Session (session_id, Some channel_id) ->
        t.conn <- Session { session_id; channel_id };
        Lwt.return_unit
    | ( Server { channel_id = cid; token; endpoint },
        `Session (session_id, Some channel_id) ) ->
        if Snowflake.(cid <> channel_id) then
          L.warn (fun m ->
              m "got unexpected channel_id while joining"
                ~fields:
                  F.
                    [
                      str "got" (Snowflake.to_string channel_id);
                      str "expected" (Snowflake.to_string cid);
                    ]);
        connect t ~channel_id ~session_id ~token ~endpoint >|= ignore
    | Session { session_id; channel_id }, `Server (token, endpoint) ->
        connect t ~channel_id ~session_id ~token ~endpoint >|= ignore
    | Live { session; mixer }, (`Server (token, endpoint) as upd)
      when should_reconnect session upd ->
        Mixer.destroy mixer;
        Session.disconnect session >>= fun () ->
        let session_id = Session.session_id session in
        let channel_id = Session.channel_id session in
        connect t ~channel_id ~session_id ~token ~endpoint >|= ignore
    | Live { session; mixer }, `Session (session_id, Some channel_id)
      when should_reconnect session (`Session (session_id, channel_id)) ->
        Mixer.destroy mixer;
        Session.disconnect session >>= fun () ->
        let token = Session.token session in
        let endpoint = Session.endpoint session in
        connect t ~channel_id ~session_id ~token ~endpoint >|= ignore
    | Live { session; mixer }, `Session (_, None) ->
        L.dbg (fun m -> m "got session leave on live call, disconneting...");
        t.conn <- Detached;
        Mixer.destroy mixer;
        Session.disconnect session
    | Live _, _ ->
        notify_req t (Ok ());
        Lwt.return_unit
    | Detached, `Session (_, None) ->
        L.dbg (fun m -> m "got session leave on detached session, ignoring...");
        Lwt.return_unit
    | Detached, _ ->
        L.err (fun m -> m "getting voice events on detached call?????");
        failwith "unimplemented"

  let join t ~channel_id =
    let do_req () =
      L.dbg (fun m -> m "connecting to requested channel");
      t.conn <- Init channel_id;
      let p, u = Lwt.wait () in
      t.req <- Some u;
      Gateway.send_voice_state_update t.gw ~channel_id ~self_mute:t.muted
        ~self_deaf:t.deafened t.guild_id
      >>= fun () -> p
    in

    match session t with
    | Some (_, s) when Snowflake.(Session.channel_id s = channel_id) ->
        L.dbg (fun m -> m "already in channel, ignoring...");
        Lwt_result.return ()
    | Some (mix, sess) ->
        L.dbg (fun m -> m "call live in different channel, disconnecting...");
        Mixer.destroy mix;
        Session.disconnect sess >>= do_req
    | None -> do_req ()

  let leave t =
    match session t with
    | Some _ ->
        L.dbg (fun m -> m "call is live, disconnecting...");
        Gateway.send_voice_state_update t.gw ~self_mute:t.muted
          ~self_deaf:t.deafened t.guild_id
    | None ->
        L.dbg (fun m -> m "call detached, ignoring...");
        Lwt.return ()
end

type t = { mutable calls : Call.t Sf_map.t; gw : Gateway.t }

let user_id { gw; _ } =
  let user = Gateway.user gw in
  user.id

let server_update t srv =
  L.info (fun m -> m "srv update");
  Sf_map.get srv.E.Voice_server_update.guild_id t.calls
  |> Option.map (fun call ->
         Call.update call (`Server (srv.token, srv.endpoint)))
  |> Option.get_or ~default:Lwt.return_unit

let state_update t vst =
  L.info (fun m -> m "state update");
  match vst.E.Voice_state.guild_id with
  | Some guild_id when Snowflake.(user_id t = vst.user_id) ->
      Sf_map.get guild_id t.calls
      |> Option.map (fun call ->
             Call.update call (`Session (vst.session_id, vst.channel_id)))
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
      let call = Call.make ~gw:t.gw guild_id in
      t.calls <- Sf_map.add guild_id call t.calls;
      call