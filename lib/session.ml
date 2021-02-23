open Containers

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_Conn = Websocket.Make (Gateway_payload)
module Pl = Gateway_payload

type t = {
  mutable state : state;
  mutable ws : Ws_Conn.t;
  mutable disconnect : unit -> unit;
  ev_pipe : (Events.t, [ `r | `w ]) Lwt_pipe.t;
}

and state =
  | Greeting of conn
  | Identifying of hb
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
              | Greeting (Reconnection { seq; _ })
              | Resuming ({ seq; _ }, _) ->
                  seq
              | _ -> -1);
          lazy_str "session_id" (fun () ->
              match t.state with
              | Connected ({ id; _ }, _)
              | Greeting (Reconnection { id; _ })
              | Resuming ({ id; _ }, _) ->
                  id
              | _ -> "<not connected>");
          lazy_bool "reconn" (fun () ->
              match t.state with
              | Resuming _ | Greeting (Reconnection _) -> true
              | _ -> false);
        ]
    (module L)

let state_reducer ~(session_logger : (module Relog.Logger.S)) ~forward_event
    ~send_payload ~on_ready ~token state pl =
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
  | Greeting Fresh, Pl.Hello hb ->
      let id = Pl.Identify.make token in
      L.info (fun m ->
          m "got greeting, identifying (intents=%a)" Pl.Intents.pp id.intents);
      send_payload (Pl.Identify id);
      let hb_secs = Float.of_int hb /. 1_000. in
      let hb = make_hb hb_secs in
      `Update (Identifying hb)
  | Greeting (Reconnection ({ id; seq; _ } as info)), Hello hb ->
      L.info (fun m ->
          m "got greeting, resuming session '%s' with seq=%d" id seq);
      send_payload (Pl.make_resume ~token ~session_id:id ~seq);
      let hb_secs = Float.of_int hb /. 1_000. in
      let hb = make_hb hb_secs in
      `Update (Resuming (info, hb))
  | Greeting _conn, InvalidSession _ ->
      L.warn (fun m -> m "invalid session during initial greeting");
      `Invalidated
  | Greeting _, Heartbeat ->
      L.warn (fun m -> m "got hearbeat request during greeting, ignoring...");
      `NoUpdate
  | Greeting _, HeartbeatACK ->
      L.warn (fun m -> m "got hearbeat ack during handshake, ignoring...");
      `NoUpdate
  | Greeting Fresh, Reconnect ->
      L.warn (fun m ->
          m "got reconnection request during greeting, obliging...");
      `Retry
  | Greeting (Reconnection info), Reconnect ->
      L.warn (fun m ->
          m "got reconnection request during reconnection greeting, obliging...");
      `RetryWith info
  | Identifying hb, Dispatch (seq, Events.Ready info) ->
      let s_id = info.session_id in
      L.info (fun m ->
          m "session is ready"
            ~fields:F.[ int "version" info.v; str "id" s_id; int "seq" seq ]);
      on_ready ();
      `Update (Connected ({ id = s_id; seq }, hb))
  | (Identifying hb | Resuming (_, hb) | Connected (_, hb)), Hello new_hb ->
      L.warn (fun m -> m "got new greeting, updating heartbeat_interval");
      let hb_secs = Float.of_int new_hb /. 1_000. in
      hb.preempt ~interval:hb_secs ();
      `NoUpdate
  | Identifying hb, InvalidSession _ ->
      L.err (fun m -> m "session was invalidated while identifying");
      hb.cancel ();
      `Invalidated
  | Identifying hb, Reconnect ->
      L.warn (fun m -> m "got reconnection request while identifying");
      hb.cancel ();
      `Retry
  | (Greeting _ | Identifying _), Dispatch _ ->
      L.warn (fun m ->
          m "received dispatch during initial handshake, ignoring...");
      `NoUpdate
  | (Identifying hb | Resuming (_, hb) | Connected (_, hb)), Heartbeat ->
      L.info (fun m -> m "requested heartbeat, obliging...");
      hb.preempt ();
      `NoUpdate
  | (Identifying hb | Resuming (_, hb) | Connected (_, hb)), HeartbeatACK ->
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
      `Update (Identifying hb)
  | Resuming (info, hb), Reconnect ->
      hb.cancel ();
      `RetryWith info
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
  | Connected (({ id; seq; _ } as info), hb), Reconnect ->
      L.warn (fun m ->
          m "got reconnection request" ~fields:F.[ str "id" id; int "seq" seq ]);
      hb.cancel ();
      `RetryWith info
  | Connected (({ seq; id; _ } as info), hb), InvalidSession resume ->
      L.warn (fun m ->
          m "session invalidated" ~fields:F.[ bool "resumable" resume ]);
      hb.cancel ();
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
  let rec manager ?t () =
    let p_connected, u_connected = Lwt.wait () in
    let p_closed, u_closed = Lwt.wait () in
    let ws_conn_handler : Ws_Conn.handler =
     fun ws ->
      let disconnect () =
        Lwt.wakeup_later u_closed `Disconnect;
        Ws_Conn.close ~code:`Going_away ws
      in
      let t =
        match t with
        | Some t ->
            t.ws <- ws;
            t
        | None ->
            {
              state = Greeting Fresh;
              ws;
              ev_pipe = Lwt_pipe.create ~max_size:10 ();
              disconnect;
            }
      in
      let module L = (val session_logger t) in
      let forward_event ev =
        Lwt.async (fun () ->
            Lwt_pipe.write_exn t.ev_pipe ev
            |> Lwt.map (fun () -> L.debug (fun m -> m "wrote to events pipe")))
      in
      let send_payload =
        let bucket = Token_bucket.make ~capacity:1 2. in
        fun pl ->
          Lwt.async (fun () ->
              Token_bucket.take bucket
              |> Lwt.map (fun () -> Ws_Conn.send t.ws pl))
      in
      let on_ready () = Lwt.wakeup_later u_connected (Ok t) in
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
              Ws_Conn.close ~code:`Normal_closure t.ws;
              Lwt.wakeup_later u_closed (`Retry None)
          | `RetryWith info ->
              Ws_Conn.close ~code:`Abnormal_closure t.ws;
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
              | `Other 4000 | `Other 4007 | `Other 4009 -> `Retry None
              | `Other 4004 -> `Invalid "invalid token"
              | _ -> `Invalid "unknown"
            in
            Lwt.wakeup_later u_closed reason
    in
    let* () = Ws_Conn.create ~zlib ~handler:ws_conn_handler uri in
    let* t = p_connected in
    if Lwt.is_sleeping p_init then Lwt.wakeup_later u_init (Ok t);
    let* reason = p_closed |> Lwt_result.ok in
    match reason with
    | `Retry None ->
        t.state <- Greeting Fresh;
        manager ~t ()
    | `Retry (Some info) ->
        t.state <- Greeting (Reconnection info);
        manager ~t ()
    | `Disconnect -> Lwt_result.return ()
    | `Invalid msg -> Lwt_result.fail (`Discord msg)
  in
  Lwt.async (fun () ->
      manager ()
      |> Lwt.map (function
           | Ok () -> ()
           | Error e -> failwith (Error.to_string e)));
  p_init

let events { ev_pipe; _ } = ev_pipe

let disconnect { disconnect; _ } = disconnect ()
