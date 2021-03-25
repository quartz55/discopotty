open Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_Conn = Websocket.Make (Gateway_payload)
module Pl = Gateway_payload

module Ws = struct
  type t = t' ref

  and t' = Open of Token_bucket.t * Ws_Conn.t | Closed

  let create conn = ref (Open (Token_bucket.make ~capacity:2 1., conn))

  let send t pl =
    match !t with
    | Open (tb, conn) ->
        Lwt.(
          Token_bucket.take tb >|= fun () ->
          Ws_Conn.send conn pl;
          Ok ())
    | Closed -> Lwt.return (Error (`Msg "cannot send payload to closed ws"))

  let send_exn t pl =
    Lwt.(
      send t pl >|= function
      | Ok () -> ()
      | Error e -> failwith (Error.to_string e))

  let close ~code t =
    match !t with
    | Open (tb, conn) ->
        t := Closed;
        Token_bucket.cancel_waiting tb;
        Ws_Conn.close ~code conn
    | Closed -> ()
end

type info = { id : string; seq : int }

type ev_tx = Events.t Lwt_pipe.Writer.t

type t = {
  user : Models.User.t;
  disconnect : unit -> unit Lwt.t;
  ev_rx : Events.t Lwt_pipe.Reader.t;
  pl_tx : Pl.send Lwt_pipe.Writer.t;
}

type handshake_state =
  | Greet of conn
  | Id of hb
  | Resuming of info * ev_tx * hb

and conn = Fresh | Reconnection of info * ev_tx

and hb = {
  mutable interval : float;
  mutable preempt : ?interval:float -> unit -> unit;
  mutable ack : unit -> unit;
  mutable cancel : unit -> unit;
}

let make_heartbeat ?err fn interval =
  let err =
    Option.get_or
      ~default:(fun () -> failwith "no ACK of last heartbeat received")
      err
  in
  let stub = Fun.const () in
  let out =
    { interval; preempt = (fun ?interval:_ -> stub); ack = stub; cancel = stub }
  in
  let rec loop () =
    let open Lwt.Syntax in
    let acked = ref false in
    let () = fn () in
    let p_preempt, u_preempt = Lwt.wait () in
    let p_sleep = Lwt_unix.sleep out.interval |> Lwt.map (Fun.const `Sleep) in
    out.ack <- (fun () -> acked := true);
    out.cancel <-
      (fun () ->
        if Lwt.is_sleeping p_preempt then Lwt.wakeup_later u_preempt `Cancel);
    out.preempt <-
      (fun ?(interval = interval) () ->
        if Lwt.is_sleeping p_preempt then
          Lwt.wakeup_later u_preempt (`Preempt interval));
    let* r = Lwt.pick [ p_sleep; p_preempt ] in
    match (r, !acked) with
    | `Sleep, true -> loop ()
    | `Preempt interval, _ ->
        out.interval <- interval;
        loop ()
    | `Sleep, false ->
        err ();
        Lwt.return ()
    | `Cancel, _ -> Lwt.return ()
  in
  Lwt.async loop;
  out

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
    make_heartbeat
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
               hb.cancel ();
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
               hb.cancel ();
               e)
    | (Greet _ as st), (Heartbeat | HeartbeatACK) ->
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
        hb.preempt ~interval:hb_secs ();
        poll' st
    | ((Id hb | Resuming (_, _, hb)) as st), Pl.Heartbeat ->
        L.debug (fun m -> m "requested heartbeat, obliging...");
        hb.preempt ();
        poll' st
    | ((Id hb | Resuming (_, _, hb)) as st), Pl.HeartbeatACK ->
        L.debug (fun m -> m "got hearbeat ack");
        hb.ack ();
        poll' st
    | Id hb, Dispatch (seq, Events.Ready info) ->
        let id = info.session_id in
        L.info (fun m ->
            m "session is ready"
              ~fields:F.[ int "version" info.v; str "id" id; int "seq" seq ]);
        Lwt_result.return (`Connected (info.user, { id; seq }, hb))
    | Id _, InvalidSession _ ->
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
    | Resuming (_, _, hb), InvalidSession _ ->
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

let create_conn uri =
  let open Lwt_result.Syntax in
  let+ conn = Ws_Conn.create ~zlib:false uri in
  let p =
    Lwt_pipe.of_stream (Ws_Conn.stream conn)
    |> Lwt_pipe.Reader.map ~f:(function
         | Ws_Conn.Payload pl -> `Pl pl
         | Close code ->
             L.warn (fun m ->
                 m "gateway ws session was closed: %a" Websocket.Close_code.pp
                   code);
             `Closed code)
  in
  ( Ws.create conn,
    (p
      : [ `Pl of Pl.recv | `Closed of Websocket.Close_code.t ] Lwt_pipe.Reader.t)
  )

let create ?(on_destroy = fun _ -> ()) ?(zlib = false)
    ?(version = Versions.Gateway.V8) token uri =
  let open Lwt_result.Syntax in
  let uri = uri |> with_ws_params ~zlib ~version ~enc:`json in

  let p_init, u_init = Lwt.wait () in
  let ev_pipe = Lwt_pipe.create () in
  let pl_pipe = Lwt_pipe.create () in
  let read_exn p = Lwt_pipe.read p >|= Option.get_exn in
  let dc = ref (fun () -> Lwt.return ()) in

  let rec manage' ?session () =
    let rec connect () =
      let* ws, pipe = create_conn uri in
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
          connect ()
      | Error #Error.t as e -> Lwt.return e
    in
    let* ws, pipe, res = connect () in
    let bus =
      Lwt_pipe.Reader.merge_all
        [
          pipe |> Lwt_pipe.Reader.map ~f:(fun pl -> `Ws pl);
          pl_pipe |> Lwt_pipe.Reader.map ~f:(fun pl -> `Fwd pl);
        ]
    in
    let p_dc, u_dc = Lwt.wait () in
    let p_dc'ed, u_dc'ed = Lwt.wait () in
    (dc :=
       fun () ->
         if Lwt.is_sleeping p_dc then Lwt.wakeup_later u_dc `Dc;
         p_dc'ed);
    let info, hb =
      match res with
      | `Connected (user, info, hb) ->
          if Lwt.is_sleeping p_init then
            Lwt.wakeup_later u_init
              (Ok
                 {
                   user;
                   disconnect = (fun () -> !dc ());
                   ev_rx = ev_pipe;
                   pl_tx = pl_pipe;
                 });
          (ref info, hb)
      | `Reconnected (info, hb) -> (ref info, hb)
    in
    let rec poll' () =
      let open Lwt.Syntax in
      let pl = read_exn bus in
      let* res = Lwt.pick [ pl; p_dc ] in
      match res with
      | `Fwd pl -> Ws.send_exn ws pl >>= poll'
      | `Ws (`Pl pl) -> handle_payload pl
      | `Ws (`Closed code) ->
          L.error (fun m ->
              m "session closed with unrecoverable close code: %a"
                Websocket.Close_code.pp code);
          Lwt_result.fail
            (`Discord
              (Format.asprintf "unrecoverable close code: %a"
                 Websocket.Close_code.pp code))
      | `Dc ->
          Ws.close ~code:`Normal_closure ws;
          Lwt_pipe.close_nonblock pipe;
          Lwt_pipe.close_nonblock pl_pipe;
          Lwt_pipe.close_nonblock bus;
          Lwt.wakeup_later u_dc'ed ();
          Lwt_result.return ()
    and handle_payload = function
      | Hello new_hb ->
          L.warn (fun m -> m "got new greeting, updating heartbeat");
          hb.preempt ~interval:(float new_hb /. 1e3) ();
          poll' ()
      | Heartbeat ->
          L.debug (fun m -> m "requested hearbeat, obliging...");
          hb.preempt ();
          poll' ()
      | HeartbeatACK ->
          L.debug (fun m -> m "got heartbeat ack");
          hb.ack ();
          poll' ()
      | InvalidSession resumable ->
          L.warn (fun m -> m "session invalidated (resumable=%b)" resumable);
          let session = if resumable then Some (!info, ev_pipe) else None in
          hb.cancel ();
          Lwt_pipe.close_nonblock bus;
          (* TODO what to do in case of `false` again?? *)
          manage' ?session ()
      | Reconnect ->
          L.warn (fun m -> m "got reconnection request, obliging...");
          hb.cancel ();
          Lwt_pipe.close_nonblock bus;
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
    hb.cancel ();
    Ws.close ~code:`Going_away ws;
    Lwt_pipe.close_nonblock pipe;
    Lwt_pipe.close_nonblock pl_pipe;
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

let _send_exn { pl_tx; _ } pl = Lwt_pipe.write_exn pl_tx pl

let send_presence_update t ?since ~afk status =
  _send_exn t (Pl.make_presence_update ?since ~afk status)

let send_voice_state_update t ?channel_id ?(self_mute = false)
    ?(self_deaf = false) guild_id =
  _send_exn t
    (Pl.make_voice_state_update ?channel_id ~self_mute ~self_deaf guild_id)

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  _send_exn t (Pl.make_request_guild_members ?presences ?nonce ~q guild_id)

let disconnect { disconnect; _ } = disconnect ()
