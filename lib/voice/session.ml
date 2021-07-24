open! Disco_core.Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_conn = Disco_core.Websocket.Make (Payload)
module Pl = Payload

module Versions = struct
  type t = V4

  let to_string = function V4 -> "4"

  let to_query t = ("v", [ to_string t ])
end

module Wsd = struct
  type t = {
    tb : Token_bucket.t;
    ws : Ws_conn.t;
    rx : [ `Pl of Pl.recv | `Closed of Close_code.t ] Lwt_pipe.Reader.t;
  }

  (* TODO i'm assuming the same rate limiting of the
     main gateway applies here as well *)
  let create uri =
    let open Lwt_result.Syntax in
    let capacity = 120 in
    let rate = 1. /. (float capacity /. 60.) in
    let tb = Token_bucket.make ~capacity rate in
    let+ ws = Ws_conn.create ~zlib:false uri in
    let rx =
      Lwt_pipe.of_stream (Ws_conn.stream ws)
      |> Lwt_pipe.Reader.map ~f:(function
           | Ws_conn.Payload pl -> `Pl pl
           | Close code ->
               let code = Close_code.of_close_code_exn code in
               L.warn (fun m ->
                   m "voice ws session was closed: %a" Close_code.pp code);
               `Closed code)
    in
    { tb; ws; rx }

  let rx { rx; _ } = rx

  let is_closed { ws; _ } = Ws_conn.is_closed ws

  let send t pl =
    if is_closed t then
      Lwt.return (Error.msg "cannot send payload to closed ws")
    else
      Token_bucket.take t.tb >|= fun () ->
      Ws_conn.send t.ws pl;
      Ok ()

  let send_exn t pl =
    send t pl >|= function
    | Ok () -> ()
    | Error e -> failwith (Error.to_string e)

  let close ~code t =
    Lwt_pipe.close_nonblock t.rx;
    if is_closed t then ()
    else (
      Token_bucket.cancel_waiting t.tb;
      Ws_conn.close ~code t.ws)
end

type handshake_state =
  | Greet of conn
  | Id of Heartbeat.t
  | Resuming of Heartbeat.t * rtp
  | Establish_udp of Heartbeat.t * Udp_connection.t

and conn = Fresh | Reconnection of rtp

and rtp = { rtp : Rtp.t; mutable speaking : bool }

let secret_of_int_list l =
  Seq.of_list l |> Seq.map Char.of_int_exn |> Bytes.of_seq

let with_ws_params ~version uri =
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri [ ("v", [ Versions.to_string version ]) ]

let do_handshake ~server_id ~user_id ~session_id ~token ?rtp wsd =
  let open Lwt_result.Syntax in
  let ws_rx = Wsd.rx wsd in
  let make_hb interval =
    Heartbeat.make
      (fun nonce -> Lwt.async (fun () -> Wsd.send_exn wsd (Pl.heartbeat nonce)))
      interval
  in
  let rec poll' st =
    let* pl =
      Lwt_pipe.read ws_rx
      |> Lwt.map (function
           | Some (`Pl pl) -> Ok pl
           | Some (`Closed code) -> Error (`Closed code)
           | None -> assert false)
    in
    match (st, pl) with
    | Greet Fresh, Pl.Hello hb ->
        L.info (fun m -> m "got greeting, identifying");
        let* () =
          Wsd.send wsd (Pl.make_identify ~server_id ~user_id ~session_id ~token)
        in
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        poll' (Id hb)
    | Greet (Reconnection rtp), Hello hb ->
        L.info (fun m -> m "got greeting, resuming session '%s'" session_id);
        let* () = Wsd.send wsd (Pl.make_resume ~server_id ~session_id ~token) in
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        poll' (Resuming (hb, rtp))
    | ( ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st),
        Hello new_interval ) ->
        let hb_secs = Float.of_int new_interval /. 1e3 in
        L.warn (fun m -> m "got new greeting, updating heartbeat");
        Heartbeat.preempt ~interval:hb_secs hb;
        poll' st
    | ( ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st),
        Heartbeat_ack nonce ) ->
        L.debug (fun m -> m "got hearbeat ack for nonce '%d'" nonce);
        Heartbeat.ack nonce hb;
        poll' st
    | Resuming (hb, rtp), Resumed -> Lwt_result.return (hb, rtp)
    | Id hb, Ready ({ ip; port; ssrc; _ } as info) ->
        L.info (fun m ->
            m
              "voice ws session is ready, connecting to UDP voice server with:\n\
               %a"
              Pl.Ready.pp info);
        let* udp = Udp_connection.create ~ssrc (ip, port) |> Error.catch_lwt in
        let ip, port = Udp_connection.local_addr udp in
        let address = Unix.string_of_inet_addr ip in
        let* () =
          Wsd.send wsd
            (Pl.make_select_protocol ~address ~port ~mode:"xsalsa20_poly1305")
        in
        poll' (Establish_udp (hb, udp))
    | Establish_udp (hb, udp), Session_description desc ->
        L.info (fun m -> m "successfuly handshaked voice connection");
        let secret = secret_of_int_list desc.secret_key in
        let mode = Udp_connection.encryption_mode_of_string desc.mode in
        let crypt = Udp_connection.{ secret; mode } in
        Lwt_result.return (hb, { rtp = Rtp.make ~udp ~crypt; speaking = false })
    | st, _pl ->
        L.warn (fun m -> m "ignoring non-control payload during handshake");
        poll' st
  in
  let c = match rtp with Some s -> Reconnection s | None -> Fresh in
  poll' (Greet c)

type t = {
  version : Versions.t;
  guild_id : snowflake;
  user_id : snowflake;
  channel_id : snowflake;
  token : string;
  session_id : string;
  endpoint : string;
  op_tx : op Lwt_pipe.Writer.t;
  on_destroy : Error.t Lwt.t;
}

and op =
  | Ws of Pl.send
  | Speak of bool
  | Rtp of bigstring
  | Dc of (unit -> unit)

let create ?(version = Versions.V4) ~guild_id ~user_id ~channel_id ~session_id
    ~token endpoint =
  let open Lwt_result.Syntax in
  let uri = Uri.of_string ("wss://" ^ endpoint) |> with_ws_params ~version in

  let p_init, u_init = Lwt.wait () in
  let chan = Lwt_pipe.create () in
  let on_destroy, destroyed_u = Lwt.wait () in
  let read_exn p = Lwt_pipe.read p >|= Option.get_exn in

  let rec manage' ?rtp () =
    let rec connect () =
      let* wsd = Wsd.create uri in
      let res =
        do_handshake ~server_id:guild_id ~user_id ~session_id ~token ?rtp wsd
      in
      Lwt.bind res (function
        | Ok (hb, rtp) -> Lwt_result.return (wsd, hb, rtp)
        | Error (`Closed code) when Close_code.is_recoverable code ->
            L.warn (fun m ->
                m "recoverable close code during handshake, retrying...");
            connect ()
        | Error (`Closed `Disconnected) ->
            L.warn (fun m ->
                m "disconnected during handshake, invalid permissions?");
            Lwt.return
              (Error (`Discord "voice session disconnected during handshake"))
        | Error (`Closed _) ->
            L.error (fun m -> m "unrecoverable close code during handshake");
            assert false
        | Error #Error.t as e -> Lwt.return e)
    in
    let* wsd, hb, rtp = connect () in
    if Lwt.is_sleeping p_init then
      Lwt.wakeup_later u_init
        (Ok
           {
             version;
             guild_id;
             user_id;
             channel_id;
             token;
             session_id;
             endpoint;
             op_tx = chan;
             on_destroy;
           });

    let bus =
      Lwt_pipe.Reader.merge_all
        [
          Wsd.rx wsd |> Lwt_pipe.Reader.map ~f:(fun pl -> `Ws pl);
          chan |> Lwt_pipe.Reader.map ~f:(fun op -> `Op op);
        ]
    in
    let rec poll' () =
      let open Lwt.Syntax in
      let* task = read_exn bus in
      match task with
      | `Ws (`Pl pl) -> handle_payload pl
      | `Ws (`Closed code) when Close_code.is_recoverable code ->
          L.warn (fun m ->
              m "session closed with recoverable close code, retrying...");
          rtp.speaking <- false;
          Lwt_pipe.close_nonblock bus;
          Heartbeat.cancel hb;
          manage' ~rtp ()
      | `Ws (`Closed code) ->
          L.error (fun m ->
              m "session closed with unrecoverable close code: %a" Close_code.pp
                code);
          Lwt.return
            (Error (`Discord (Format.asprintf "%a" Close_code.pp code)))
      | `Op (Ws pl) -> Wsd.send_exn wsd pl >>= poll'
      | `Op (Speak sp) when Stdlib.(sp != rtp.speaking) ->
          rtp.speaking <- sp;
          Wsd.send_exn wsd
            (Pl.make_speaking ~ssrc:(Rtp.ssrc rtp.rtp) ~delay:0
               (if sp then 1 else 0))
          >>= poll'
      | `Op (Speak _) -> poll' ()
      | `Op (Rtp audio) -> Rtp.send_packet rtp.rtp audio >>= poll'
      | `Op (Dc k) ->
          Lwt_pipe.close_nonblock bus;
          Lwt_pipe.close_nonblock chan;
          Rtp.destroy rtp.rtp;
          Wsd.close ~code:`Normal_closure wsd;
          k ();
          Lwt.return (Ok ())
    and handle_payload = function
      | Hello new_hb ->
          L.warn (fun m -> m "got new greeting, updating heartbeat");
          Heartbeat.preempt ~interval:(Float.of_int new_hb /. 1e3) hb;
          poll' ()
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
          Rtp.set_crypt rtp.rtp Udp_connection.{ secret; mode };
          poll' ()
      | Heartbeat_ack nonce ->
          Heartbeat.ack nonce hb;
          poll' ()
      | Speaking _ | Resumed | Client_disconnect ->
          L.debug (fun m -> m "ignoring speaking/resumed/clientdisconnect");
          poll' ()
    in
    poll' () >|= fun out ->
    Heartbeat.cancel hb;
    Rtp.destroy rtp.rtp;
    Wsd.close ~code:`Going_away wsd;
    Lwt_pipe.close_nonblock bus;
    Lwt_pipe.close_nonblock chan;
    out
  in
  Lwt.async (fun () ->
      manage' ()
      |> Lwt.map (function
           | Ok () -> ()
           | Error e when Lwt.is_sleeping p_init ->
               Lwt.wakeup_later u_init (Error e)
           | Error e -> Lwt.wakeup_later destroyed_u e));
  p_init

let is_dead { op_tx; _ } = Lwt_pipe.is_closed op_tx

let version { version; _ } = version

let guild_id { guild_id; _ } = guild_id

let user_id { user_id; _ } = user_id

let channel_id { channel_id; _ } = channel_id

let token { token; _ } = token

let session_id { session_id; _ } = session_id

let endpoint { endpoint; _ } = endpoint

let send_exn { op_tx; _ } op =
  Lwt_pipe.write op_tx op >>= fun ok ->
  if ok then Lwt.return_unit
  else (
    L.err (fun m -> m "trying to interact with dead session");
    Lwt.fail (Failure "trying to interact with dead session"))

let speak s t = send_exn t (Speak s)

let start_speaking = speak true

let stop_speaking = speak false

let send_rtp t audio = send_exn t (Rtp audio)

let disconnect t =
  let p, u = Lwt.wait () in
  send_exn t (Dc (Lwt.wakeup_later u)) >>= fun () -> p
