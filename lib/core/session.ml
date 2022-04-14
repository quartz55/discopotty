open! Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field
module Ws_conn = Websocket.Make (Gateway_payload)
module Pl = Gateway_payload
module Token_bucket = Lf_token_bucket

exception Handshake_error of string
exception Dead

module Ws = struct
  type t = {
    sw : Switch.t;
    id_bucket : Token_bucket.t;
    bucket : Token_bucket.t;
    conn : Ws_conn.t;
  }

  exception Closed

  let send t pl =
    if Ws_conn.is_closed t.conn then raise Closed;
    let bucket = match pl with Pl.Identify _ -> t.id_bucket | _ -> t.bucket in
    Token_bucket.take ~sw:t.sw bucket;
    Ws_conn.send t.conn pl

  let read t = Ws_conn.read t.conn
  let read_exn t = Ws_conn.read t.conn |> Option.get_exn

  let close ~code t =
    if not @@ Ws_conn.is_closed t.conn then Ws_conn.close ~code t.conn

  (* docs: clients are allowed to send 120 gateway commands every 60 seconds *)
  let create ~sw ~net ~id_bucket uri =
    let capacity = 120 in
    let rate = 1. /. (float capacity /. 60.) in
    let bucket = Token_bucket.make ~capacity rate in
    let conn = Ws_conn.create ~sw ~net ~zlib:false ~enc:`json uri in
    let t = { sw; id_bucket; bucket; conn } in
    Switch.on_release sw (fun () -> close ~code:`Going_away t);
    t
end

type heartbeat = unit Heartbeat.t

type t = {
  user : Models.User.t;
  shard : shard;
  ops : op Eio.Stream.t;
  mutable state : state;
  id_bucket : Token_bucket.t;
  version : Versions.Gateway.t;
  mutable fwd : Events.t -> unit;
  token : string;
  uri : Uri.t;
  sw : Switch.t;
}

and op = Ws of Pl.send | Dc of (unit -> unit)
and info = { id : string; mutable seq : int }
and state = Dead | Alive of { ws : Ws.t; hb : heartbeat; info : info }
and shard = Single | Sharded of int * int

type handshake_state =
  | Greet of info option
  | Id of heartbeat
  | Resuming of info * heartbeat

let with_ws_params ?(zlib = false) ~enc ~version uri =
  (* let enc = match enc with `json -> "json" | `etf -> "etf" in *)
  let enc = match enc with `json -> "json" in
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri
    ([ ("encoding", [ enc ]); ("v", [ Versions.Gateway.to_string version ]) ]
    @ if zlib then [ ("compress", [ "zlib-stream" ]) ] else [])

let handshake ~sw ~token ~fwd ?reconn ws =
  let module L =
    (val Relog.clone (module L) ~fields:F.[ bool "handshaking" true ])
  in
  let make_hb interval =
    Heartbeat.make ~sw
      ~t:(module Heartbeat.Unit)
      (fun () ->
        L.debug (fun m ->
            m "sending heartbeat" ~fields:F.[ float "interval" interval ]);
        Ws.send ws Pl.heartbeat)
      interval
  in
  let rec loop st =
    match Ws.read_exn ws with
    | Ws_conn.Payload pl -> next st pl
    | Close code -> Error (`Closed (Close_code.of_close_code_exn code))
  and next st pl =
    match (st, pl) with
    | Greet None, Pl.Hello hb ->
        let id = Pl.Identify.make token in
        L.info (fun m ->
            m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
        Ws.send ws (Pl.Identify id);
        let hb_secs = Float.of_int hb /. 1_000. in
        let hb = make_hb hb_secs in
        loop (Id hb)
    | Greet (Some info), Hello hb ->
        L.info (fun m ->
            m "got greeting, resuming session '%s' with seq=%d" info.id info.seq);
        Ws.send ws (Pl.make_resume ~token ~session_id:info.id ~seq:info.seq);
        let hb_secs = Float.of_int hb /. 1_000. in
        let hb = make_hb hb_secs in
        loop (Resuming (info, hb))
    | (Greet _ as st), (Heartbeat | Heartbeat_ack) ->
        (* safe to ignore heartbeat related payloads during greeting
           as we'll send an heartbeat as soon as we can *)
        loop st
    | (Greet _ as st), _ ->
        L.err (fun m ->
            m
              "!!!PROTOCOL VIOLATION!!! got something else other than Hello \
               when greeting");
        loop st
    | ((Id hb | Resuming (_, hb)) as st), Pl.Hello new_hb ->
        L.info (fun m -> m "got new greeting, updating heartbeat");
        let hb_secs = Float.of_int new_hb /. 1_000. in
        Heartbeat.preempt ~interval:hb_secs hb;
        loop st
    | ((Id hb | Resuming (_, hb)) as st), Pl.Heartbeat ->
        L.debug (fun m -> m "requested heartbeat, obliging...");
        Heartbeat.preempt hb;
        loop st
    | ((Id hb | Resuming (_, hb)) as st), Pl.Heartbeat_ack ->
        L.debug (fun m -> m "got hearbeat ack");
        Heartbeat.ack hb ();
        loop st
    | Id hb, Dispatch (seq, Events.Ready info) ->
        let id = info.session_id in
        L.info (fun m ->
            m "session is ready"
              ~fields:F.[ int "version" info.v; str "id" id; int "seq" seq ]);
        Ok (`Connected (info.user, { id; seq }, hb))
    | Id _, Invalid_session _ ->
        L.err (fun m -> m "session was invalidated while identifying");
        Error `Invalidated
    | (Id _ as st), Dispatch _ ->
        L.warn (fun m ->
            m "ignoring non ready dispatch event during identification");
        loop st
    | Resuming (info, hb), Dispatch (seq, Resumed) ->
        L.info (fun m -> m "successfully resumed session '%s'" info.id);
        Ok (`Reconnected ({ info with seq }, hb))
    | Resuming (info, hb), Dispatch (seq, ev) ->
        L.debug (fun m ->
            m "forwarding replayed event"
              ~fields:F.[ int "seq" seq; int "event_seq" seq ]);
        fwd ev;
        loop (Resuming ({ info with seq }, hb))
    | Resuming (_, hb), Invalid_session _ ->
        let rand_wait =
          let r = Random.(run (float_range 1. 5.)) in
          (* only need precision up to ms *)
          float (truncate (r *. 1e3)) /. 1e3
        in
        L.warn (fun m ->
            m "couldn't resume session, waiting for %fs and identifying..."
              rand_wait);
        Eio_unix.sleep rand_wait;
        let id = Pl.Identify.make token in
        L.info (fun m ->
            m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
        Ws.send ws (Pl.Identify id);
        loop (Id hb)
    | _, Reconnect ->
        Ws.close ~code:`Abnormal_closure ws;
        Error `Retry
  in
  match loop (Greet reconn) with
  | Ok o -> `Ok o
  | Error `Retry -> `Retry
  | Error (`Closed c) when Close_code.is_recoverable c -> `Retry
  | Error (`Closed _) ->
      L.err (fun m -> m "session closed during handshake");
      raise @@ Handshake_error "unrecoverable close code"
  | Error `Invalidated ->
      L.err (fun m ->
          m "session invalidated during handshake, invalid permissions?");
      raise @@ Handshake_error "invalid session"

let connect ~sw ~net ~id_bucket ~token ~fwd ?info uri =
  let rec loop () =
    let ws = Ws.create ~sw ~net ~id_bucket uri in
    handshake ~sw ~token ~fwd ?reconn:info ws |> function
    | `Ok res -> (ws, res)
    | `Retry ->
        L.warn (fun m -> m "retrying handshake");
        loop ()
  in
  loop ()

let user { user; _ } = user
let is_dead { state; _ } = match state with Dead -> true | Alive _ -> false

let send_exn t op =
  if is_dead t then raise Dead;
  Eio.Stream.add t.ops op

let send_presence_update t ?since ~afk status =
  send_exn t (Ws (Pl.make_presence_update ?since ~afk status))

let send_voice_state_update t ?channel_id ?(self_mute = false)
    ?(self_deaf = false) guild_id =
  send_exn t
    (Ws (Pl.make_voice_state_update ?channel_id ~self_mute ~self_deaf guild_id))

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  send_exn t (Ws (Pl.make_request_guild_members ?presences ?nonce ~q guild_id))

let disconnect t =
  if not @@ is_dead t then (
    let p, u = Promise.create () in
    send_exn t (Dc (Promise.resolve u));
    Promise.await p)

let kill t =
  t.state <- Dead;
  match Switch.get_error t.sw with
  | None -> Switch.fail t.sw Exit
  | Some _ -> ()

let manage ~sw ~net t =
  let drain () =
    match t.state with
    | Dead -> ()
    | Alive { ws; hb; _ } ->
        Heartbeat.cancel hb;
        Ws.close ~code:`Abnormal_closure ws;
        t.state <- Dead
  in
  let reconnect ?info () =
    drain ();
    let new_state =
      match
        connect ~sw ~net ~id_bucket:t.id_bucket ~token:t.token ~fwd:t.fwd ?info
          t.uri
      with
      | ws, (`Connected (_, info, hb) | `Reconnected (info, hb)) ->
          Alive { ws; hb; info }
    in
    t.state <- new_state
  in
  let rec loop () =
    match t.state with
    | Dead -> ()
    | Alive { ws; hb; info } -> next ~ws ~hb ~info
  and next ~ws ~hb ~info =
    let bus =
      Fiber.first
        (fun () -> `Ws (Ws.read_exn ws))
        (fun () -> `Op (Eio.Stream.take t.ops))
    in
    match bus with
    | `Op (Ws pl) ->
        Ws.send ws pl;
        loop ()
    | `Ws (Payload pl) -> handle_payload ~hb ~info pl
    | `Ws (Close code) ->
        L.error (fun m ->
            m "session closed with unrecoverable close code: %a"
              Websocket.Close_code.pp code);
        Error.raise
          (`Discord
            (Format.asprintf "unrecoverable close code: %a"
               Websocket.Close_code.pp code))
    | `Op (Dc k) ->
        Ws.close ~code:`Normal_closure ws;
        Fiber.yield ();
        k ()
  and handle_payload ~hb ~info = function
    | Hello new_hb ->
        L.warn (fun m -> m "got new greeting, updating heartbeat");
        Heartbeat.preempt ~interval:(float new_hb /. 1e3) hb;
        loop ()
    | Heartbeat ->
        L.debug (fun m -> m "requested hearbeat, obliging...");
        Heartbeat.preempt hb;
        loop ()
    | Heartbeat_ack ->
        L.debug (fun m -> m "got heartbeat ack");
        Heartbeat.ack hb ();
        loop ()
    | Invalid_session resumable ->
        L.warn (fun m -> m "session invalidated (resumable=%b)" resumable);
        let info = if resumable then Some info else None in
        (* let state = match connect *)
        (* TODO what to do in case of `false`let state = again?? *)
        reconnect ?info ();
        loop ()
    | Reconnect ->
        L.warn (fun m -> m "got reconnection request, obliging...");
        reconnect ~info ();
        loop ()
    | Dispatch (seq, Ready _) | Dispatch (seq, Resumed) ->
        L.warn (fun m ->
            m "ignoring handshake related payloads on established session");
        info.seq <- seq;
        loop ()
    | Dispatch (seq, ev) ->
        info.seq <- seq;
        t.fwd ev;
        loop ()
  in
  Fun.protect ~finally:drain loop

let create ~sw ~net ?(zlib = false) ?(version = Versions.Gateway.V8) ~fwd
    ~id_bucket token uri =
  let uri = uri |> with_ws_params ~zlib ~version ~enc:`json in

  let p, u = Promise.create () in
  let spawn () : no_return =
    Switch.run @@ fun sw ->
    let user, state =
      match connect ~sw ~net ~id_bucket ~token ~fwd uri with
      | ws, `Connected (user, info, hb) -> (user, Alive { ws; hb; info })
      | _, `Reconnected _ ->
          Error.(raise @@ of_discord "reconnected on creation?")
    in
    let ops = Eio.Stream.create 0 in
    let t =
      {
        user;
        shard = Single;
        ops;
        state;
        id_bucket;
        version;
        fwd;
        token;
        uri;
        sw;
      }
    in
    Switch.on_release sw (fun () -> kill t);
    Promise.resolve u t;
    manage ~sw ~net t;
    (* cleanup: stops any fibres still running (e.g. heartbeats) *)
    raise Exit
  in
  Fiber.fork ~sw (fun () -> match spawn () with _ -> . | exception Exit -> ());
  Promise.await p
