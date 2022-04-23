open! Disco_core.Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field
module Pl = Payload
module Heartbeat = Disco_core.Heartbeat
module Token_bucket = Lf_token_bucket

exception Handshake_error of string
exception Dead

module Versions = struct
  type t = V4

  let to_string = function V4 -> "4"
  let to_query t = ("v", [ to_string t ])
end

module Ws = struct
  module Ws_conn = Disco_core.Websocket.Make (Payload)

  type t = { sw : Switch.t; bucket : Token_bucket.t; conn : Ws_conn.t }
  type frame = Payload of Payload.recv | Close of Close_code.t

  exception Closed

  let send t pl =
    if Ws_conn.is_closed t.conn then raise Closed;
    Token_bucket.take ~sw:t.sw t.bucket;
    Ws_conn.send t.conn pl

  let read t =
    Ws_conn.read t.conn
    |> Option.map (function
         | Ws_conn.Close c -> Close (Close_code.of_close_code_exn c)
         | Payload p -> Payload p)

  let read_exn t = read t |> Option.get_exn

  let close ~code t =
    if not @@ Ws_conn.is_closed t.conn then Ws_conn.close ~code t.conn

  (* TODO @quartz55: i'm assuming the same rate limiting
     of the main gateway applies here as well *)
  (* docs: clients are allowed to send 120 gateway commands every 60 seconds *)
  let create ~sw ~net uri =
    let capacity = 120 in
    let rate = 1. /. (float capacity /. 60.) in
    let bucket = Token_bucket.make ~capacity rate in
    let conn = Ws_conn.create ~sw ~net ~zlib:false ~enc:`json uri in
    let t = { sw; bucket; conn } in
    Switch.on_release sw (fun () -> close ~code:`Going_away t);
    t
end

type heartbeat = int Heartbeat.t

type t = {
  guild_id : snowflake;
  user_id : snowflake;
  channel_id : snowflake;
  token : string;
  session_id : string;
  endpoint : string;
  uri : Uri.t;
  ops : op Eio.Stream.t;
  mutable state : state;
  version : Versions.t;
  sw : Switch.t;
}

and op =
  | Ws of Pl.send
  | Speak of bool
  | Rtp of bigstring
  | Dc of (unit -> unit)

and state = Dead | Alive of info
and info = { ws : Ws.t; hb : heartbeat; rtp : Rtp.t; mutable speaking : bool }

type handshake_state =
  | Greet of Rtp.t option
  | Id of heartbeat
  | Resuming of heartbeat * Rtp.t
  | Establish_udp of heartbeat * Udp_connection.t

let secret_of_int_list l =
  Seq.of_list l |> Seq.map Char.of_int_exn |> Bytes.of_seq

let with_ws_params ~version uri =
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri [ ("v", [ Versions.to_string version ]) ]

let handshake ~sw ~net ~server_id ~user_id ~session_id ~token ?reconn ws =
  let module L =
    (val Relog.clone (module L) ~fields:F.[ bool "handshaking" true ])
  in
  let make_hb interval =
    Heartbeat.make ~sw
      ~t:(module Heartbeat.Int)
      (fun nonce ->
        L.debug (fun m ->
            m "sending heartbeat" ~fields:F.[ float "interval" interval ]);
        Ws.send ws (Pl.heartbeat nonce))
      interval
  in
  let rec loop st =
    match Ws.read_exn ws with
    | Payload pl -> next st pl
    | Close code -> Error (`Closed code)
  and next st pl =
    match (st, pl) with
    | Greet None, Pl.Hello hb ->
        L.info (fun m -> m "got greeting, identifying");
        Ws.send ws (Pl.make_identify ~server_id ~user_id ~session_id ~token);
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        loop (Id hb)
    | Greet (Some rtp), Hello hb ->
        L.info (fun m -> m "got greeting, resuming session '%s'" session_id);
        Ws.send ws (Pl.make_resume ~server_id ~session_id ~token);
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        loop (Resuming (hb, rtp))
    | ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st), Hello new_hb
      ->
        L.info (fun m -> m "got new greeting, updating heartbeat");
        let hb_secs = Float.of_int new_hb /. 1_000. in
        Heartbeat.preempt ~interval:hb_secs hb;
        loop st
    | ( ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st),
        Heartbeat_ack nonce ) ->
        L.debug (fun m -> m "got hearbeat ack for nonce '%d'" nonce);
        Heartbeat.ack hb nonce;
        loop st
    | Resuming (hb, rtp), Resumed -> Ok (hb, rtp)
    | Id hb, Ready ({ ip; port; ssrc; _ } as info) ->
        L.info (fun m ->
            m
              "voice ws session is ready, connecting to UDP voice server with:\n\
               %a"
              Pl.Ready.pp info);
        let udp = Udp_connection.create ~sw ~net ~ssrc (ip, port) in
        let (`Udp (ip, port)) = Udp_connection.local_addr udp in
        let address = Format.sprintf "%a" Eio.Net.Ipaddr.pp ip in
        Ws.send ws
          (Pl.make_select_protocol ~address ~port ~mode:"xsalsa20_poly1305");
        loop (Establish_udp (hb, udp))
    | Establish_udp (hb, udp), Session_description desc ->
        L.info (fun m -> m "successfuly handshaked voice connection");
        let secret = secret_of_int_list desc.secret_key in
        let mode = Udp_connection.encryption_mode_of_string desc.mode in
        let crypt = Udp_connection.{ secret; mode } in
        Ok (hb, Rtp.make ~udp ~crypt)
    | st, (Speaking _ | Client_disconnect) ->
        L.warn (fun m -> m "ignoring non-control payload during handshake");
        loop st
    | _, _ ->
        L.err (fun m -> m "!!!PROTOCOL VIOLATION!!!");
        assert false
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

let connect ~sw ~net ~guild_id ~user_id ~session_id ~token ?info uri =
  let rec loop () =
    let ws = Ws.create ~sw ~net uri in
    handshake ws ~sw ~net ~server_id:guild_id ~user_id ~session_id ~token
      ?reconn:info
    |> function
    | `Ok (hb, rtp) -> Alive { ws; hb; rtp; speaking = false }
    | `Retry ->
        L.warn (fun m -> m "retrying handshake");
        loop ()
  in
  loop ()

let is_dead { state; _ } = match state with Dead -> true | Alive _ -> false

let send_exn t op =
  if is_dead t then raise Dead;
  Eio.Stream.add t.ops op

let speak s t = send_exn t (Speak s)
let start_speaking = speak true
let stop_speaking = speak false
let send_rtp t audio = send_exn t (Rtp audio)

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
      connect ~sw ~net ~guild_id:t.guild_id ~user_id:t.user_id
        ~session_id:t.session_id ~token:t.token ?info t.uri
    in
    t.state <- new_state
  in
  let rec loop () = match t.state with Dead -> () | Alive st -> next st
  and next st =
    let bus =
      Fiber.first
        (fun () -> `Ws (Ws.read_exn st.ws))
        (fun () -> `Op (Eio.Stream.take t.ops))
    in
    match bus with
    | `Op (Ws pl) ->
        Ws.send st.ws pl;
        loop ()
    | `Ws (Payload pl) -> handle_payload st pl
    | `Ws (Close code) when Close_code.is_recoverable code ->
        L.warn (fun m ->
            m "session closed with recoverable close code, retrying...");
        reconnect ~info:st.rtp ();
        loop ()
    | `Ws (Close code) ->
        L.error (fun m ->
            m "session closed with unrecoverable close code: %a" Close_code.pp
              code);
        assert false
    | `Op (Speak sp) when not @@ Bool.equal sp st.speaking ->
        st.speaking <- sp;
        Ws.send st.ws
          (Pl.make_speaking ~ssrc:(Rtp.ssrc st.rtp) ~delay:0
             (if sp then 1 else 0));
        loop ()
    | `Op (Speak _) -> loop ()
    | `Op (Rtp audio) ->
        Rtp.send_packet st.rtp audio;
        loop ()
    | `Op (Dc k) ->
        Ws.close ~code:`Normal_closure st.ws;
        k ()
  and handle_payload st = function
    | Hello new_hb ->
        L.warn (fun m -> m "got new greeting, updating heartbeat");
        Heartbeat.preempt ~interval:(float new_hb /. 1e3) st.hb;
        loop ()
    | Ready info ->
        L.error (fun m ->
            m "got ready on established session, new voice server??@.%a"
              Pl.Ready.pp info);
        assert false
    | Session_description desc ->
        L.warn (fun m ->
            m "got session description, updating...@.%a"
              Pl.Session_description.pp desc);
        let secret = secret_of_int_list desc.secret_key in
        let mode = Udp_connection.encryption_mode_of_string desc.mode in
        Rtp.set_crypt st.rtp Udp_connection.{ secret; mode };
        loop ()
    | Heartbeat_ack nonce ->
        Heartbeat.ack st.hb nonce;
        loop ()
    | Speaking _ | Resumed | Client_disconnect ->
        L.debug (fun m -> m "ignoring speaking/resumed/clientdisconnect");
        loop ()
  in
  Fun.protect ~finally:drain loop

let create ~sw ~net ?(version = Versions.V4) ~guild_id ~user_id ~channel_id
    ~session_id ~token endpoint =
  let uri = Uri.of_string ("wss://" ^ endpoint) |> with_ws_params ~version in

  let p, u = Promise.create () in
  let spawn () : no_return =
    Switch.run @@ fun sw ->
    let state = connect ~sw ~net ~guild_id ~user_id ~session_id ~token uri in
    let ops = Eio.Stream.create 0 in
    let t =
      {
        guild_id;
        user_id;
        channel_id;
        session_id;
        ops;
        state;
        version;
        token;
        endpoint;
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

let guild_id { guild_id; _ } = guild_id
let user_id { user_id; _ } = user_id
let channel_id { channel_id; _ } = channel_id
let session_id { session_id; _ } = session_id
let token { token; _ } = token
let endpoint { endpoint; _ } = endpoint
