open Globals

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
        Lwt.async (fun () ->
            Token_bucket.take tb |> Lwt.map (fun () -> Ws_Conn.send conn pl));
        true
    | Closed -> false

  let send_exn t pl =
    if not (send t pl) then failwith "cannot send payload to closed ws"

  let close ~code t =
    match !t with
    | Open (tb, conn) ->
        Token_bucket.cancel_waiting tb;
        Ws_Conn.close ~code conn
    | Closed -> ()
end

type t = {
  mutable state : state;
  mutable ws : Ws.t;
  mutable disconnect : unit -> unit;
  ev_pipe : (Events.t, [ `r | `w ]) Lwt_pipe.t;
  ev_stream : Events.t Lwt_pipe.Reader.t;
  writer : Pl.send Lwt_pipe.Writer.t;
}

and state =
  | Greet of conn
  | Id of hb
  | Resuming of session_info * hb
  | Connected of session_info * hb

and conn = Fresh | Reconnection of session_info

and session_info = { id : string; seq : int }

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

let cancel_hb = function
  | Id hb | Resuming (_, hb) | Connected (_, hb) -> hb.cancel ()
  | _ -> ()

let with_ws_params ?(zlib = false) ~enc ~version uri =
  (* let enc = match enc with `json -> "json" | `etf -> "etf" in *)
  let enc = match enc with `json -> "json" in
  let uri = Uri.with_path uri "/" in
  Uri.with_query uri
    ([ ("encoding", [ enc ]); ("v", [ Versions.Gateway.to_string version ]) ]
    @ if zlib then [ ("compress", [ "zlib-stream" ]) ] else [])

let session_logger : t -> (module Relog.Logger.S) =
 fun t ->
  Relog.clone
    ~fields:
      F.
        [
          lazy_int "seq" (fun () ->
              match t.state with
              | Connected ({ seq; _ }, _)
              | Greet (Reconnection { seq; _ })
              | Resuming ({ seq; _ }, _) ->
                  seq
              | _ -> -1);
          lazy_str "session_id" (fun () ->
              match t.state with
              | Connected ({ id; _ }, _)
              | Greet (Reconnection { id; _ })
              | Resuming ({ id; _ }, _) ->
                  id
              | _ -> "<not connected>");
          lazy_bool "reconn" (fun () ->
              match t.state with
              | Resuming _ | Greet (Reconnection _) -> true
              | _ -> false);
        ]
    (module L)

let state_reducer ~(session_logger : (module Relog.Logger.S)) ~forward_event
    ~send_payload ~(on_ready : ?user:Models.User.t -> unit -> unit) ~token state
    pl =
  let module L = (val session_logger) in
  let make_hb interval =
    make_heartbeat
      (fun () ->
        L.info (fun m ->
            m "sending heartbeat" ~fields:F.[ float "hb_interval" interval ]);
        send_payload Pl.heartbeat)
      interval
  in
  match (state, pl) with
  | Greet Fresh, Pl.Hello hb ->
      let id = Pl.Identify.make token in
      L.info (fun m ->
          m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
      send_payload (Pl.Identify id);
      let hb_secs = Float.of_int hb /. 1_000. in
      let hb = make_hb hb_secs in
      `Update (Id hb)
  | Greet (Reconnection ({ id; seq; _ } as info)), Hello hb ->
      L.info (fun m ->
          m "got greeting, resuming session '%s' with seq=%d" id seq);
      send_payload (Pl.make_resume ~token ~session_id:id ~seq);
      let hb_secs = Float.of_int hb /. 1_000. in
      let hb = make_hb hb_secs in
      `Update (Resuming (info, hb))
  | Greet _conn, InvalidSession _ ->
      L.warn (fun m -> m "invalid session during initial greeting");
      `Invalidated
  | Greet _, Heartbeat ->
      L.warn (fun m -> m "got hearbeat request during greeting, ignoring...");
      `NoUpdate
  | Greet _, HeartbeatACK ->
      L.warn (fun m -> m "got hearbeat ack during handshake, ignoring...");
      `NoUpdate
  | Greet Fresh, Reconnect ->
      L.warn (fun m ->
          m "got reconnection request during greeting, obliging...");
      `Retry
  | Greet (Reconnection info), Reconnect ->
      L.warn (fun m ->
          m "got reconnection request during reconnection greeting, obliging...");
      `RetryWith info
  | Id hb, Dispatch (seq, Events.Ready info) ->
      let s_id = info.session_id in
      L.info (fun m ->
          m "session is ready"
            ~fields:F.[ int "version" info.v; str "id" s_id; int "seq" seq ]);
      on_ready ~user:info.user ();
      `Update (Connected ({ id = s_id; seq }, hb))
  | (Id hb | Resuming (_, hb) | Connected (_, hb)), Hello new_hb ->
      L.warn (fun m -> m "got new greeting, updating heartbeat");
      let hb_secs = Float.of_int new_hb /. 1_000. in
      hb.preempt ~interval:hb_secs ();
      `NoUpdate
  | Id _hb, InvalidSession _ ->
      L.err (fun m -> m "session was invalidated while identifying");
      `Invalidated
  | Id _hb, Reconnect ->
      L.warn (fun m -> m "got reconnection request while identifying");
      `Retry
  | (Greet _ | Id _), Dispatch _ ->
      L.warn (fun m ->
          m "received dispatch during initial handshake, ignoring...");
      `NoUpdate
  | (Id hb | Resuming (_, hb) | Connected (_, hb)), Heartbeat ->
      L.info (fun m -> m "requested heartbeat, obliging...");
      hb.preempt ();
      `NoUpdate
  | (Id hb | Resuming (_, hb) | Connected (_, hb)), HeartbeatACK ->
      L.debug (fun m -> m "got hearbeat ack");
      hb.ack ();
      `NoUpdate
  | Resuming (_, hb), InvalidSession _ ->
      L.warn (fun m ->
          m "couldn't resume session, waiting for %fs and identifying..." 3.4);
      let id = Pl.Identify.make token in
      L.info (fun m ->
          m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
      Lwt.async (fun () ->
          Lwt_unix.sleep 3.4
          |> Lwt.map (fun () -> send_payload (Pl.Identify id)));
      `Update (Id hb)
  | Resuming (info, _hb), Reconnect -> `RetryWith info
  | Resuming (({ id; _ } as i), hb), Dispatch (e_seq, Resumed) ->
      on_ready ();
      L.info (fun m -> m "successfully resumed session '%s'" id);
      `Update (Connected ({ i with seq = e_seq }, hb))
  | Resuming (({ seq; _ } as i), hb), Dispatch (e_seq, e) ->
      L.info (fun m ->
          m "forwarding replayed event"
            ~fields:F.[ int "seq" seq; int "event_seq" e_seq ]);
      forward_event e;
      `Update (Connected ({ i with seq = e_seq }, hb))
  | Connected (({ id; seq; _ } as info), _hb), Reconnect ->
      L.warn (fun m ->
          m "got reconnection request" ~fields:F.[ str "id" id; int "seq" seq ]);
      `RetryWith info
  | Connected (({ seq; id; _ } as info), _hb), InvalidSession resume ->
      L.warn (fun m ->
          m "session invalidated" ~fields:F.[ bool "resumable" resume ]);
      if resume then (
        L.info (fun m -> m "will try to resume session '%s' on seq=%d" id seq);
        `RetryWith info)
      else `Retry
  | Connected (({ seq; _ } as i), hb), Dispatch (e_seq, e) ->
      L.info (fun m ->
          m "forwarding event"
            ~fields:F.[ int "seq" seq; int "event_seq" e_seq ]);
      forward_event e;
      `Update (Connected ({ i with seq = e_seq }, hb))

let create ?(zlib = false) ?(version = Versions.Gateway.V8) token uri =
  let open Lwt_result.Syntax in
  let uri = uri |> with_ws_params ~zlib ~version ~enc:`json in

  let p_init, u_init = Lwt.wait () in
  let send_pipe = Lwt_pipe.create () in
  let rec manager ?t () =
    let p_connected, u_connected = Lwt.wait () in
    let p_closed, u_closed = Lwt.wait () in
    let ws_conn_handler : Ws_Conn.handler =
     fun ws ->
      let ws = Ws.create ws in
      let disconnect () = Lwt.wakeup_later u_closed `Disconnect in
      let t =
        match t with
        | Some t ->
            Ws.close ~code:`Normal_closure t.ws;
            t.ws <- ws;
            t.disconnect <- disconnect;
            t
        | None ->
            let p = Lwt_pipe.create ~max_size:10 () in
            {
              state = Greet Fresh;
              ws;
              ev_pipe = p;
              ev_stream = p;
              disconnect;
              writer = send_pipe;
            }
      in
      let module L = (val session_logger t) in
      let send_payload = Ws.send_exn t.ws in
      let forward_event = function
        | Events.Unsupported (n, _) ->
            L.warn (fun m -> m "unsupported event: %s" n)
        | ev ->
            Lwt.async (fun () ->
                Lwt_pipe.write_exn t.ev_pipe ev
                |> Lwt.map (fun () ->
                       L.debug (fun m -> m "wrote to events pipe")))
      in
      let on_ready ?user () = Lwt.wakeup_later u_connected (Ok (t, user)) in
      let invalidated = ref false in
      let handle_payload =
        let f' =
          state_reducer
            ~session_logger:(module L)
            ~forward_event ~send_payload ~on_ready ~token
        in
        fun pl ->
          match f' t.state pl with
          | `NoUpdate -> ()
          | `Update st -> t.state <- st
          | `Retry ->
              Ws.close ~code:`Normal_closure t.ws;
              Lwt.wakeup_later u_closed (`Retry None)
          | `RetryWith info ->
              Ws.close ~code:`Abnormal_closure t.ws;
              Lwt.wakeup_later u_closed (`Retry (Some info))
          | `Invalidated -> invalidated := true
      in
      fun frame ->
        match (frame, !invalidated) with
        | Payload pl, false -> handle_payload pl
        | Payload _, true -> ()
        | Close code, _ ->
            L.warn (fun m ->
                m "session was closed with code %a" Websocket.Close_code.pp code);
            let reason =
              (* Make (or extend) type for Discord error codes *)
              match code with
              | `Normal_closure | `Other 4006 | `Other 4007 -> `Retry None
              | `Other 4004 -> `Invalid "invalid token"
              | _ -> `Invalid "unknown"
            in
            Lwt.wakeup_later u_closed reason
    in
    let* () = Ws_Conn.create ~zlib ~handler:ws_conn_handler uri in
    let* t, user = p_connected in
    let () =
      match user with
      | Some user when Lwt.is_sleeping p_init ->
          Lwt.wakeup_later u_init (Ok (t, user))
      | _ -> ()
    in
    let rec manage' () =
      let open Lwt.Syntax in
      let forward_pl =
        Lwt_pipe.read send_pipe
        |> Lwt.map (function
             | Some pl -> `Forward_payload pl
             | None -> failwith "!BUG! send_pipe should never have been closed")
      in
      let* reason = Lwt.pick [ forward_pl; p_closed ] in
      match reason with
      | `Forward_payload pl ->
          let* () = Lwt.wrap (fun () -> Ws.send_exn t.ws pl) in
          manage' ()
      | `Retry None ->
          cancel_hb t.state;
          t.state <- Greet Fresh;
          manager ~t ()
      | `Retry (Some info) ->
          cancel_hb t.state;
          t.state <- Greet (Reconnection info);
          manager ~t ()
      | `Disconnect ->
          cancel_hb t.state;
          Ws.close ~code:`Going_away t.ws;
          let* () = Lwt_pipe.close t.ev_pipe in
          Lwt_result.return ()
      | `Invalid msg ->
          cancel_hb t.state;
          Lwt_result.fail (`Discord msg)
    in
    manage' ()
  in
  Lwt.async (fun () ->
      manager ()
      |> Lwt.map (function
           | Ok () -> ()
           | Error _ as err when Lwt.is_sleeping p_init ->
               Lwt.wakeup_later u_init err
           | Error e -> failwith (Error.to_string e)));
  p_init

let events { ev_stream; _ } = ev_stream

let _send_exn { writer; _ } pl = Lwt_pipe.write_exn writer pl

let send_presence_update t ?since ~afk status =
  _send_exn t (Pl.make_presence_update ?since ~afk status)

let send_voice_state_update t ?channel_id ~self_mute ~self_deaf guild_id =
  _send_exn t
    (Pl.make_voice_state_update ?channel_id ~self_mute ~self_deaf guild_id)

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  _send_exn t (Pl.make_request_guild_members ?presences ?nonce ~q guild_id)

let disconnect { disconnect; _ } = disconnect ()
