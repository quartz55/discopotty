open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_Conn = Websocket.Make (Voice_payload)
module Pl = Voice_payload

module Ws = struct
  type t = t' ref

  and t' = Open of Token_bucket.t * Ws_Conn.t | Closed

  let create conn = ref (Open (Token_bucket.make ~capacity:1 2., conn))

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
        Token_bucket.cancel_waiting tb;
        Ws_Conn.close ~code conn
    | Closed -> ()
end

module Close_code = struct
  type discord =
    [ `Unknown_op
    | `Invalid_payload
    | `Not_authenticated
    | `Authentication_failed
    | `Already_authenticated
    | `Invalid_session
    | `Session_timeout
    | `Server_not_found
    | `Unknown_protocol
    | `Disconnected
    | `Voice_server_crashed
    | `Unknown_encryption_mode ]

  type t = [ Websocket.Close_code.standard | discord ]

  let is_discord = function #discord -> true | _ -> false

  let is_std = function #Websocket.Close_code.standard -> true | _ -> false

  let to_int = function
    | #Websocket.Close_code.standard as c -> Websocket.Close_code.to_int c
    | `Unknown_op -> 4001
    | `Invalid_payload -> 4002
    | `Not_authenticated -> 4003
    | `Authentication_failed -> 4004
    | `Already_authenticated -> 4005
    | `Invalid_session -> 4006
    | `Session_timeout -> 4009
    | `Server_not_found -> 4011
    | `Unknown_protocol -> 4012
    | `Disconnected -> 4014
    | `Voice_server_crashed -> 4015
    | `Unknown_encryption_mode -> 4016

  let of_close_code_exn = function
    | #Websocket.Close_code.standard as c -> c
    | `Other 4001 -> `Unknown_op
    | `Other 4002 -> `Invalid_payload
    | `Other 4003 -> `Not_authenticated
    | `Other 4004 -> `Authentication_failed
    | `Other 4005 -> `Already_authenticated
    | `Other 4006 -> `Invalid_session
    | `Other 4009 -> `Session_timeout
    | `Other 4011 -> `Server_not_found
    | `Other 4012 -> `Unknown_protocol
    | `Other 4014 -> `Disconnected
    | `Other 4015 -> `Voice_server_crashed
    | `Other 4016 -> `Unknown_encryption_mode
    | `Other c -> failwith (Printf.sprintf "unknown voice close code: %d" c)

  let pp =
    let pp' fmt = function
      | `Unknown_op -> Format.fprintf fmt "invalid opcode"
      | `Invalid_payload ->
          Format.fprintf fmt "invalid payload while identifying"
      | `Not_authenticated -> Format.fprintf fmt "not authenticated"
      | `Authentication_failed -> Format.fprintf fmt "authentication failed"
      | `Already_authenticated -> Format.fprintf fmt "already authenticated"
      | `Invalid_session -> Format.fprintf fmt "session is no longer valid"
      | `Session_timeout -> Format.fprintf fmt "session timed out"
      | `Server_not_found -> Format.fprintf fmt "voice server not found"
      | `Unknown_protocol -> Format.fprintf fmt "unknown protocol"
      | `Disconnected -> Format.fprintf fmt "disconnected"
      | `Voice_server_crashed -> Format.fprintf fmt "voice server crashed"
      | `Unknown_encryption_mode -> Format.fprintf fmt "unknown encryption mode"
    in
    fun fmt t ->
      match t with
      | #Websocket.Close_code.standard as c -> Websocket.Close_code.pp fmt c
      | #discord as t -> Format.fprintf fmt "(%d %a)" (to_int t) pp' t

  let is_recoverable = function
    | `Unknown_op | `Invalid_payload | `Not_authenticated
    | `Authentication_failed | `Already_authenticated | `Unknown_protocol
    | `Disconnected | `Unknown_encryption_mode
    | #Websocket.Close_code.standard ->
        false
    | _ -> true
end

type t = {
  disconnect : unit -> unit Lwt.t;
  ws_writer : Pl.send Lwt_pipe.Writer.t;
  udp_writer : bytes Lwt_pipe.Writer.t;
}

type hb = {
  mutable interval : float;
  mutable preempt : ?interval:float -> unit -> unit;
  mutable ack : int -> unit;
  mutable cancel : unit -> unit;
}

type handshake_state =
  | Greet of conn
  | Id of hb
  | Resuming of hb * session
  | Establish_udp of hb * Udp_connection.t

and conn = Fresh | Reconnection of session

and session = {
  udp : Udp_connection.t;
  mutable mode : string;
  mutable secret : bytes;
}

let make_heartbeat ?err fn interval =
  let err =
    Option.get_or
      ~default:(fun () -> failwith "no ACK of last heartbeat received")
      err
  in
  let out =
    {
      interval;
      preempt = (fun ?interval:_ -> ignore);
      ack = ignore;
      cancel = ignore;
    }
  in
  let gen_nonce =
    let st = Random.State.make_self_init () in
    fun () -> Random.State.bits st
  in
  let rec loop () =
    let open Lwt.Syntax in
    let acked = ref false in
    let nonce = gen_nonce () in
    let () = fn nonce in
    let p_preempt, u_preempt = Lwt.wait () in
    let p_sleep = Lwt_unix.sleep out.interval |> Lwt.map (Fun.const `Sleep) in
    out.ack <- (fun n -> if not !acked then acked := nonce = n);
    out.cancel <- (fun () -> Lwt.wakeup_later u_preempt `Cancel);
    out.preempt <-
      (fun ?(interval = interval) () ->
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

let secret_of_int_list l =
  Seq.of_list l |> Seq.map Char.of_int_exn |> Bytes.of_seq

let with_ws_params ~version uri =
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri [ ("v", [ Versions.Voice.to_string version ]) ]

let do_handshake ~server_id ~user_id ~session_id ~token ?session ws pl_pipe =
  let open Lwt_result.Syntax in
  let make_hb interval =
    make_heartbeat
      (fun nonce -> Lwt.async (fun () -> Ws.send_exn ws (Pl.heartbeat nonce)))
      interval
  in
  let rec poll' st =
    let* pl =
      Lwt_pipe.read pl_pipe
      |> Lwt.map (function
           | Some (`Pl pl) -> Ok pl
           | Some (`Closed code) -> Error (`Closed code)
           | None -> assert false)
    in
    match (st, pl) with
    | Greet Fresh, Pl.Hello hb ->
        L.info (fun m -> m "got greeting, identifying");
        let* () =
          Ws.send ws (Pl.make_identify ~server_id ~user_id ~session_id ~token)
        in
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        poll' (Id hb)
    | Greet (Reconnection session), Hello hb ->
        L.info (fun m -> m "got greeting, resuming session '%s'" session_id);
        let* () = Ws.send ws (Pl.make_resume ~server_id ~session_id ~token) in
        let hb_secs = Float.of_int hb /. 1e3 in
        let hb = make_hb hb_secs in
        poll' (Resuming (hb, session))
    | ( ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st),
        Hello new_interval ) ->
        let hb_secs = Float.of_int new_interval /. 1e3 in
        L.warn (fun m -> m "got new greeting, updating heartbeat");
        hb.preempt ~interval:hb_secs ();
        poll' st
    | ( ((Id hb | Resuming (hb, _) | Establish_udp (hb, _)) as st),
        HeartbeatACK nonce ) ->
        L.debug (fun m -> m "got hearbeat ack for nonce '%d'" nonce);
        hb.ack nonce;
        poll' st
    | Resuming (hb, session), Resumed -> Lwt_result.return (hb, session)
    | Id hb, Ready ({ ip; port; ssrc; _ } as info) ->
        L.info (fun m ->
            m
              "voice ws session is ready, connecting to UDP voice server with:\n\
               %a"
              Pl.Ready.pp info);
        let* udp = Udp_connection.create ~ssrc (ip, port) |> Error.catch_lwt in
        let address, port = udp.ext_addr in
        let* () =
          Ws.send ws
            (Pl.make_select_protocol ~address ~port ~mode:"xsalsa20_poly1305")
        in
        poll' (Establish_udp (hb, udp))
    | Establish_udp (hb, udp), SessionDescription desc ->
        L.info (fun m -> m "successfuly handshaked voice connection");
        let secret = secret_of_int_list desc.secret_key in
        Lwt_result.return (hb, { udp; mode = desc.mode; secret })
    | st, _pl ->
        L.warn (fun m -> m "ignoring non-control payload during handshake");
        poll' st
  in
  let c = match session with Some s -> Reconnection s | None -> Fresh in
  poll' (Greet c)

let create_conn uri =
  let open Lwt_result.Syntax in
  let p = Lwt_pipe.create () in
  let p_conn, u_conn = Lwt.wait () in
  let* () =
    Ws_Conn.create ~zlib:false
      ~handler:(fun ws ->
        let ws = Ws.create ws in
        Lwt.wakeup_later u_conn (Ok ws);
        function
        | Payload pl -> Lwt.async (fun () -> Lwt_pipe.write_exn p (`Pl pl))
        | Close code ->
            let code = Close_code.of_close_code_exn code in
            L.warn (fun m ->
                m "voice ws session was closed: %a" Close_code.pp code);
            Lwt.async (fun () ->
                let w = Lwt_pipe.write_exn p (`Closed code) in
                Lwt.bind w (fun () -> Lwt_pipe.close p)))
      uri
  in
  let+ ws = p_conn in
  (ws, (p : [ `Pl of Pl.recv | `Closed of Close_code.t ] Lwt_pipe.Reader.t))

let create ?(on_destroy = fun _ -> ()) ?(version = Versions.Voice.V4) ~server_id
    ~user_id ~session_id ~token endpoint =
  let open Lwt_result.Syntax in
  let uri = Uri.of_string ("wss://" ^ endpoint) |> with_ws_params ~version in

  let p_init, u_init = Lwt.wait () in
  let ws_writer = Lwt_pipe.create () in
  let udp_writer = Lwt_pipe.create () in
  let read_exn p = Lwt_pipe.read p |> Lwt.map Option.get_exn in
  let dc = ref (fun () -> Lwt.return ()) in

  let rec manage' ?session () =
    let rec connect () =
      let* ws, pipe = create_conn uri in
      let res =
        do_handshake ~server_id ~user_id ~session_id ~token ?session ws pipe
      in
      Lwt.bind res (function
        | Ok (hb, session) -> Lwt_result.return (ws, pipe, hb, session)
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
    let* ws, pipe, hb, session = connect () in
    let p_dc, u_dc = Lwt.wait () in
    let p_dc'ed, u_dc'ed = Lwt.wait () in
    (dc :=
       fun () ->
         Lwt.wakeup_later u_dc `Dc;
         p_dc'ed);
    (if Lwt.is_sleeping p_init then
     let t = { disconnect = (fun () -> !dc ()); ws_writer; udp_writer } in
     Lwt.wakeup_later u_init (Ok t));

    let rec poll' () =
      let open Lwt.Syntax in
      let pl = read_exn pipe |> Lwt.map (fun ws -> `Ws ws) in
      let fwd = read_exn ws_writer |> Lwt.map (fun pl -> `Fwd pl) in
      let* res = Lwt.choose [ pl; fwd; p_dc ] in
      match res with
      | `Fwd pl ->
          let* () = Ws.send_exn ws pl in
          poll' ()
      | `Ws (`Pl pl) -> handle_payload pl
      | `Ws (`Closed code) when Close_code.is_recoverable code ->
          L.warn (fun m ->
              m "session closed with recoverable close code, retrying...");
          manage' ~session ()
      | `Ws (`Closed code) ->
          L.error (fun m ->
              m "session closed with unrecoverable close code: %a" Close_code.pp
                code);
          let* () = Udp_connection.close session.udp in
          Lwt.return
            (Error (`Discord (Format.asprintf "%a" Close_code.pp code)))
      | `Dc ->
          Lwt_pipe.close_nonblock pipe;
          Lwt_pipe.close_nonblock ws_writer;
          let* () = Lwt_pipe.close udp_writer in
          let* () = Udp_connection.close session.udp in
          Ws.close ~code:`Normal_closure ws;
          Lwt.wakeup_later u_dc'ed ();
          Lwt.return (Ok ())
    and handle_payload = function
      | Hello new_hb ->
          L.warn (fun m -> m "got new greeting, updating heartbeat");
          hb.preempt ~interval:(Float.of_int new_hb /. 1e3) ();
          poll' ()
      | Ready info ->
          L.error (fun m ->
              m "got ready on established session, new voice server??@.%a"
                Pl.Ready.pp info);
          assert false
      | SessionDescription desc ->
          L.warn (fun m ->
              m "got session description, updating...@.%a"
                Pl.SessionDescription.pp desc);
          let secret = secret_of_int_list desc.secret_key in
          session.mode <- desc.mode;
          session.secret <- secret;
          poll' ()
      | HeartbeatACK nonce ->
          hb.ack nonce;
          poll' ()
      | Speaking | Resumed | ClientDisconnect ->
          L.debug (fun m -> m "ignoring speaking/resumed/clientdisconnect");
          poll' ()
    in
    Lwt.bind (poll' ()) (fun e ->
        let open Lwt.Infix in
        hb.cancel ();
        Ws.close ~code:`Going_away ws;
        Lwt_pipe.close_nonblock pipe;
        Lwt_pipe.close_nonblock ws_writer;
        Lwt_pipe.close_nonblock udp_writer;
        Udp_connection.close session.udp >|= fun () -> e)
  in
  Lwt.async (fun () ->
      manage' ()
      |> Lwt.map (function
           | Ok () -> ()
           | Error e when Lwt.is_sleeping p_init ->
               Lwt.wakeup_later u_init (Error e)
           | Error e -> on_destroy e));
  p_init

let disconnect { disconnect; _ } = disconnect ()

(* let send_voice_data { ws_writer; udp_writer; _ } = *)
