open! Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_conn = Websocket.Make (Gateway_payload)
module Pl = Gateway_payload

module Ws = struct
  type t = t' ref

  and t' =
    | Open of {
        id_bucket : Token_bucket.t;
        bucket : Token_bucket.t;
        conn : Ws_conn.t;
      }
    | Closed

  (* docs: clients are allowed to send 120 gateway commands every 60 seconds *)
  let create ~id_bucket conn =
    let capacity = 120 in
    let rate = 1. /. (float capacity /. 60.) in
    let bucket = Token_bucket.make ~capacity rate in
    ref (Open { id_bucket; bucket; conn })

  let send t pl =
    match !t with
    | Open t ->
        let bucket =
          match pl with Pl.Identify _ -> t.id_bucket | _ -> t.bucket
        in
        Token_bucket.take bucket >|= fun () ->
        Ws_conn.send t.conn pl;
        Ok ()
    | Closed -> Lwt.return (Error.msg "cannot send payload to closed ws")

  let send_exn t pl =
    Lwt.(
      send t pl >|= function
      | Ok () -> ()
      | Error e -> failwith (Error.to_string e))

  let close ~code t =
    match !t with
    | Open { bucket; conn; _ } ->
        t := Closed;
        Token_bucket.cancel_waiting bucket;
        Ws_conn.close ~code conn
    | Closed -> ()
end

type info = { id : string; seq : int }

type ev_tx = Events.t Lwt_pipe.Writer.t

type t = {
  user : Models.User.t;
  shard : shard option;
  ev_rx : Events.t Lwt_pipe.Reader.t;
  op_tx : op Lwt_pipe.Writer.t;
}

and op = Ws of Pl.send | Dc of (unit -> unit)

and shard = int * int

type handshake_state =
  | Greet of conn
  | Id of Heartbeat.t
  | Resuming of info * ev_tx * Heartbeat.t

and conn = Fresh | Reconnection of info * ev_tx

let with_ws_params ?(zlib = false) ~enc ~version uri =
  (* let enc = match enc with `json -> "json" | `etf -> "etf" in *)
  let enc = match enc with `json -> "json" in
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri
    ([ ("encoding", [ enc ]); ("v", [ Versions.Gateway.to_string version ]) ]
    @ if zlib then [ ("compress", [ "zlib-stream" ]) ] else [])

let do_handshake ~token ?reconn ws pl_rx =
  let module L =
  (val Relog.clone (module L) ~fields:F.[ bool "handshaking" true ])
  in
  let open Lwt_result.Syntax in
  let make_hb interval =
    Heartbeat.make
      (fun () ->
        L.debug (fun m ->
            m "sending heartbeat" ~fields:F.[ float "interval" interval ]);
        Lwt.async (fun () -> Ws.send_exn ws Pl.heartbeat))
      interval
  in
  let rec poll' st =
    let* pl =
      Lwt_pipe.read pl_rx >|= function
      | Some (`Pl pl) -> Ok pl
      | Some (`Closed code) -> Error (`Closed code)
      | None -> assert false
    in
    match (st, pl) with
    | Greet Fresh, Pl.Hello hb ->
        let id = Pl.Identify.make token in
        L.info (fun m ->
            m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
        let* () = Ws.send ws (Pl.Identify id) in
        let hb_secs = Float.of_int hb /. 1_000. in
        let hb = make_hb hb_secs in
        poll' (Id hb)
        |> Lwt_result.map_err (fun e ->
               Heartbeat.cancel hb;
               e)
    | Greet (Reconnection (info, ev_tx)), Hello hb ->
        L.info (fun m ->
            m "got greeting, resuming session '%s' with seq=%d" info.id info.seq);
        let* () =
          Ws.send ws (Pl.make_resume ~token ~session_id:info.id ~seq:info.seq)
        in
        let hb_secs = Float.of_int hb /. 1_000. in
        let hb = make_hb hb_secs in
        poll' (Resuming (info, ev_tx, hb))
        |> Lwt_result.map_err (fun e ->
               Heartbeat.cancel hb;
               e)
    | (Greet _ as st), (Heartbeat | Heartbeat_ack) ->
        (* safe to ignore heartbeat related payloads during greeting
           as we'll send an heartbeat as soon as we can *)
        poll' st
    | (Greet _ as st), _ ->
        L.err (fun m ->
            m
              "!!!PROTOCOL VIOLATION!!! got something else other than Hello \
               when greeting");
        poll' st
    | ((Id hb | Resuming (_, _, hb)) as st), Pl.Hello new_hb ->
        L.info (fun m -> m "got new greeting, updating heartbeat");
        let hb_secs = Float.of_int new_hb /. 1_000. in
        Heartbeat.preempt ~interval:hb_secs hb;
        poll' st
    | ((Id hb | Resuming (_, _, hb)) as st), Pl.Heartbeat ->
        L.debug (fun m -> m "requested heartbeat, obliging...");
        Heartbeat.preempt hb;
        poll' st
    | ((Id hb | Resuming (_, _, hb)) as st), Pl.Heartbeat_ack ->
        L.debug (fun m -> m "got hearbeat ack");
        Heartbeat.ack hb;
        poll' st
    | Id hb, Dispatch (seq, Events.Ready info) ->
        let id = info.session_id in
        L.info (fun m ->
            m "session is ready"
              ~fields:F.[ int "version" info.v; str "id" id; int "seq" seq ]);
        Lwt_result.return (`Connected (info.user, { id; seq }, hb))
    | Id _, Invalid_session _ ->
        L.err (fun m -> m "session was invalidated while identifying");
        Lwt_result.fail `Invalidated
    | (Id _ as st), Dispatch _ ->
        L.warn (fun m ->
            m "ignoring non ready dispatch event during identification");
        poll' st
    | Resuming (info, _, hb), Dispatch (seq, Resumed) ->
        L.info (fun m -> m "successfully resumed session '%s'" info.id);
        Lwt_result.return (`Reconnected ({ info with seq }, hb))
    | Resuming (info, ev_tx, hb), Dispatch (seq, ev) ->
        L.debug (fun m ->
            m "forwarding replayed event"
              ~fields:F.[ int "seq" seq; int "event_seq" seq ]);
        let* () = Lwt_pipe.write_exn ev_tx ev |> Error.catch_lwt in
        poll' (Resuming ({ info with seq }, ev_tx, hb))
    | Resuming (_, _, hb), Invalid_session _ ->
        let rand_wait =
          let r = Random.(run (float_range 1. 5.)) in
          (* only need precision up to ms *)
          float (truncate (r *. 1e3)) /. 1e3
        in
        L.warn (fun m ->
            m "couldn't resume session, waiting for %fs and identifying..."
              rand_wait);
        let id = Pl.Identify.make token in
        L.info (fun m ->
            m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
        let* () =
          Lwt_unix.sleep rand_wait >>= fun () -> Ws.send ws (Pl.Identify id)
        in
        poll' (Id hb)
    | _, Reconnect -> Lwt_result.fail `Retry
  in
  let c =
    match reconn with
    | Some (info, ev_tx) -> Reconnection (info, ev_tx)
    | None -> Fresh
  in
  poll' (Greet c)

(* TODO refactor this into Ws module *)
let create_conn ~id_bucket uri =
  let open Lwt_result.Syntax in
  let+ conn = Ws_conn.create ~zlib:false uri in
  let p =
    Lwt_pipe.of_stream (Ws_conn.stream conn)
    |> Lwt_pipe.Reader.map ~f:(function
         | Ws_conn.Payload pl -> `Pl pl
         | Close code ->
             L.warn (fun m ->
                 m "gateway ws session was closed: %a" Websocket.Close_code.pp
                   code);
             `Closed code)
  in
  ( Ws.create ~id_bucket conn,
    (p
      : [ `Pl of Pl.recv | `Closed of Websocket.Close_code.t ] Lwt_pipe.Reader.t)
  )

let create ?(on_destroy = fun _ -> ()) ?(zlib = false)
    ?(version = Versions.Gateway.V8) ~id_bucket token uri =
  let open Lwt_result.Syntax in
  let uri = uri |> with_ws_params ~zlib ~version ~enc:`json in

  let p_init, u_init = Lwt.wait () in
  let ev_pipe = Lwt_pipe.create () in
  let op_pipe = Lwt_pipe.create () in
  let read_exn p = Lwt_pipe.read p >|= Option.get_exn in

  let rec manage' ?session () =
    let rec connect () =
      let* ws, pipe = create_conn ~id_bucket uri in
      do_handshake ~token ?reconn:session ws pipe >>= function
      | Ok res -> Lwt_result.return (ws, pipe, res)
      | Error (`Closed _) ->
          L.warn (fun m -> m "session closed during handshake");
          Lwt_result.fail (`Discord "unrecoverable close code during handshake")
      | Error `Invalidated ->
          L.warn (fun m ->
              m "session invalidated during handshake, invalid permissions?");
          Lwt_result.fail (`Discord "unrecoverable close code during handshake")
      | Error `Retry ->
          L.warn (fun m -> m "retrying handshake");
          Ws.close ~code:`Abnormal_closure ws;
          Lwt_pipe.close_nonblock pipe;
          connect ()
      | Error #Error.t as e -> Lwt.return e
    in
    let* ws, ws_rx, res = connect () in
    let bus =
      Lwt_pipe.Reader.merge_all
        [
          ws_rx |> Lwt_pipe.Reader.map ~f:(fun pl -> `Ws pl);
          op_pipe |> Lwt_pipe.Reader.map ~f:(fun op -> `Op op);
        ]
    in
    Lwt_pipe.link_close ws_rx ~after:bus;
    let info, hb =
      match res with
      | `Connected (user, info, hb) ->
          if Lwt.is_sleeping p_init then
            Lwt.wakeup_later u_init
              (Ok { user; ev_rx = ev_pipe; op_tx = op_pipe; shard = None });
          (ref info, hb)
      | `Reconnected (info, hb) -> (ref info, hb)
    in
    let rec poll' () =
      let open Lwt.Syntax in
      read_exn bus >>= function
      | `Op (Ws pl) -> Ws.send_exn ws pl >>= poll'
      | `Ws (`Pl pl) -> handle_payload pl
      | `Ws (`Closed code) ->
          L.error (fun m ->
              m "session closed with unrecoverable close code: %a"
                Websocket.Close_code.pp code);
          Lwt_result.fail
            (`Discord
              (Format.asprintf "unrecoverable close code: %a"
                 Websocket.Close_code.pp code))
      | `Op (Dc k) ->
          Ws.close ~code:`Normal_closure ws;
          Lwt_pipe.close_nonblock op_pipe;
          Lwt_pipe.close_nonblock bus;
          k ();
          Lwt_result.return ()
    and handle_payload = function
      | Hello new_hb ->
          L.warn (fun m -> m "got new greeting, updating heartbeat");
          Heartbeat.preempt ~interval:(float new_hb /. 1e3) hb;
          poll' ()
      | Heartbeat ->
          L.debug (fun m -> m "requested hearbeat, obliging...");
          Heartbeat.preempt hb;
          poll' ()
      | Heartbeat_ack ->
          L.debug (fun m -> m "got heartbeat ack");
          Heartbeat.ack hb;
          poll' ()
      | Invalid_session resumable ->
          L.warn (fun m -> m "session invalidated (resumable=%b)" resumable);
          let session = if resumable then Some (!info, ev_pipe) else None in
          Heartbeat.cancel hb;
          Lwt_pipe.close_nonblock bus;
          Ws.close ~code:`Abnormal_closure ws;
          (* TODO what to do in case of `false` again?? *)
          manage' ?session ()
      | Reconnect ->
          L.warn (fun m -> m "got reconnection request, obliging...");
          Heartbeat.cancel hb;
          Lwt_pipe.close_nonblock bus;
          Ws.close ~code:`Abnormal_closure ws;
          manage' ~session:(!info, ev_pipe) ()
      | Dispatch (seq, Ready _) | Dispatch (seq, Resumed) ->
          L.warn (fun m ->
              m "ignoring handshake related payloads on established session");
          info := { !info with seq };
          poll' ()
      | Dispatch (seq, ev) ->
          info := { !info with seq };
          Lwt_pipe.write ev_pipe ev >>= fun ok ->
          if ok then poll' () else Lwt_result.return ()
    in
    poll' () >>= fun out ->
    Heartbeat.cancel hb;
    Ws.close ~code:`Normal_closure ws;
    Lwt_pipe.close_nonblock op_pipe;
    Lwt_pipe.close_nonblock bus;
    Lwt_pipe.close ev_pipe >|= fun () -> out
  in
  Lwt.async (fun () ->
      manage' () >|= function
      | Ok () -> ()
      | Error e when Lwt.is_sleeping p_init -> Lwt.wakeup_later u_init (Error e)
      | Error e -> on_destroy e);
  p_init

let events { ev_rx; _ } = ev_rx

let user { user; _ } = user

let is_dead { op_tx; _ } = Lwt_pipe.is_closed op_tx

let send_exn { op_tx; _ } op =
  Lwt_pipe.write op_tx op >>= fun ok ->
  if ok then Lwt.return_unit
  else (
    L.err (fun m -> m "trying to interact with dead session");
    Lwt.fail (Failure "trying to interact with dead session"))

let send_presence_update t ?since ~afk status =
  send_exn t (Ws (Pl.make_presence_update ?since ~afk status))

let send_voice_state_update t ?channel_id ?(self_mute = false)
    ?(self_deaf = false) guild_id =
  send_exn t
    (Ws (Pl.make_voice_state_update ?channel_id ~self_mute ~self_deaf guild_id))

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  send_exn t (Ws (Pl.make_request_guild_members ?presences ?nonce ~q guild_id))

let disconnect t =
  if is_dead t then Lwt.return_unit
  else
    let p, u = Lwt.wait () in
    send_exn t (Dc (Lwt.wakeup_later u)) >>= fun () -> p
