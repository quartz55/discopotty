open! Disco_core.Globals
module Snowflake = Disco_models.Snowflake
module E = Disco_core.Events
module Gateway = Disco_core.Gateway
module F = Relog.Field
module L = (val Relog.logger ~namespace:__MODULE__ ())

exception Detached
exception Timed_out

type srv_info = { token : string; endpoint : string }
type sess_info = { channel_id : snowflake; session_id : string }

type t = {
  gw : Gateway.t;
  guild_id : snowflake;
  mixer : Mixer.t;
  conn : conn Atomic.t;
  mutable muted : bool;
  mutable deafened : bool;
  evs : events Eio.Stream.t;
}

and conn =
  | Detached of unit req
  | Init of { cid : snowflake; req : unit req; info : conn_info }
  | Live of Session.t

and conn_info = Empty | Got_srv of srv_info | Got_sess of sess_info

and events =
  | Req_join of snowflake * unit req
  | Req_play of Audio_stream.t * unit req
  | Req_stop of unit req
  | Gw_srv of srv_info
  | Gw_sess of sess_info
  | Gw_dc

and 'a req = ('a, exn) result -> unit

let wait_for_session conn =
  let b = Backoff.create () in
  let rec loop () =
    match Atomic.get conn with
    | Live session -> session
    | Init ({ req = o_req; _ } as init) as st ->
        let p, u = Promise.create () in
        if
          Atomic.compare_and_set conn st
            (Init
               {
                 init with
                 req =
                   (fun res ->
                     o_req res;
                     Promise.resolve u ());
               })
        then Promise.await p
        else Backoff.once b;
        loop ()
    | Detached o_req as st ->
        let p, u = Promise.create () in
        if
          Atomic.compare_and_set conn st
            (Detached
               (fun res ->
                 o_req res;
                 Promise.resolve u ()))
        then Promise.await p
        else Backoff.once b;
        loop ()
  in
  loop ()

let stub _ = ()

let manage ~net ~dmgr t =
  Switch.run @@ fun sw ->
  let user_id = (Gateway.user t.gw).id in
  let connect ~srv ~sess =
    Session.create ~sw ~net ~guild_id:t.guild_id ~user_id
      ~channel_id:sess.channel_id ~session_id:sess.session_id ~token:srv.token
      srv.endpoint
  in
  let mixer_thread () =
    Switch.on_release sw (fun () -> Mixer.destroy t.mixer);
    Eio.Domain_manager.run dmgr @@ fun () ->
    let play i f =
      let s = wait_for_session t.conn in
      Session.start_speaking s;
      Session.send_rtp s f;
      i < 15
    in
    let stop () = wait_for_session t.conn |> Session.stop_speaking in
    Mixer.run ~play ~stop t.mixer
  in
  let b = Backoff.create () in
  let rec main_thread () =
    let ev = Eio.Stream.take t.evs in
    handle ev
  and cas ~ev ost nst fn =
    if Atomic.compare_and_set t.conn ost nst then fn ()
    else (
      Backoff.once b;
      handle ev)
  and handle ev =
    match (Atomic.get t.conn, ev) with
    | (Detached o_req as st), Req_join (cid, n_req) ->
        let req res =
          o_req res;
          n_req res
        in
        cas ~ev st (Init { cid; req; info = Empty }) @@ fun () ->
        (* TODO timeout *)
        L.dbg (fun m -> m "join request while detached");
        Gateway.send_voice_state_update t.gw ~channel_id:cid ~self_mute:t.muted
          ~self_deaf:t.deafened t.guild_id;
        main_thread ()
    | Detached _, (Req_play (_, req) | Req_stop req) ->
        req (Error Detached);
        main_thread ()
    | Detached _, (Gw_dc | Gw_sess _ | Gw_srv _) ->
        L.warn (fun m -> m "gateway event on detached call");
        main_thread ()
    | Init { info = Got_sess sess; req; _ }, Gw_srv srv
    | Init { info = Got_srv srv; req; _ }, Gw_sess sess ->
        (* any race conditions with mixer that require a CAS & extra state ? *)
        let res =
          match connect ~srv ~sess with
          | sess ->
              Atomic.set t.conn @@ Live sess;
              Ok ()
          | exception exn ->
              Atomic.set t.conn @@ Detached stub;
              Error exn
        in
        req res;
        main_thread ()
    | (Init ({ info = Got_sess _ | Empty; _ } as init) as st), Gw_sess sess ->
        cas ~ev st (Init { init with info = Got_sess sess }) @@ main_thread
    | (Init ({ info = Got_srv _ | Empty; _ } as init) as st), Gw_srv srv ->
        cas ~ev st (Init { init with info = Got_srv srv }) @@ main_thread
    | (Init { req; _ } as st), Gw_dc ->
        cas ~ev st (Detached stub) @@ fun () -> req @@ Error Detached
    | (Init ({ cid; req = o_req; _ } as init) as st), Req_join (n_cid, n_req)
      when Snowflake.(cid = n_cid) ->
        let req res =
          o_req res;
          n_req res
        in
        cas ~ev st (Init { init with req }) @@ main_thread
    | ( (Init ({ req = o_req; _ } as init) as st),
        (Req_join _ | Req_play _ | Req_stop _) ) ->
        let req res =
          o_req res;
          Eio.Stream.add t.evs ev
        in
        cas ~ev st (Init { init with req }) @@ main_thread
    | Live sess, Req_join (cid, req)
      when Snowflake.(cid = Session.channel_id sess) ->
        req @@ Ok ()
    | (Live sess as st), Req_join _ ->
        cas ~ev st (Detached stub) @@ fun () ->
        L.dbg (fun m -> m "join request while live, switching channels");
        Session.disconnect sess;
        handle ev
    | Live _, Req_play (audio, req) ->
        let res =
          Result.guard (fun () -> Mixer.play ~s:(Mixer.stream audio) t.mixer)
        in
        req res;
        main_thread ()
    | Live _, Req_stop req ->
        let res = Result.guard (fun () -> Mixer.stop t.mixer) in
        req res;
        main_thread ()
    | (Live sess as st), Gw_sess nsess
      when Snowflake.(Session.channel_id sess <> nsess.channel_id)
           || String.(Session.session_id sess <> nsess.session_id) ->
        let srv = Session.{ token = token sess; endpoint = endpoint sess } in
        cas ~ev st
          (Init { req = stub; cid = nsess.channel_id; info = Got_srv srv })
        @@ fun () ->
        L.dbg (fun m -> m "new voice session info, reconnecting");
        Session.disconnect sess;
        handle ev
    | (Live sess as st), Gw_dc ->
        cas ~ev st (Detached stub) @@ fun () ->
        L.dbg (fun m -> m "disconnected");
        Session.disconnect sess;
        main_thread ()
    | Live _, (Gw_sess _ | Gw_srv _) -> main_thread ()
  in
  Fiber.both main_thread mixer_thread;
  Mixer.destroy t.mixer;
  raise Exit

let make ~sw ~net ~dmgr ?(muted = false) ?(deafened = false) ~guild_id gw =
  let t =
    {
      gw;
      guild_id;
      mixer = Mixer.create ();
      conn = Atomic.make @@ Detached stub;
      muted;
      deafened;
      evs = Eio.Stream.create max_int;
    }
  in
  Fiber.fork ~sw (fun () -> try manage ~net ~dmgr t with Exit -> ());
  t

let update_server ~token ~endpoint t =
  Eio.Stream.add t.evs @@ Gw_srv { token; endpoint }

let update_session ?channel_id ~session_id t =
  let ev =
    match channel_id with
    | None -> Gw_dc
    | Some channel_id -> Gw_sess { channel_id; session_id }
  in
  Eio.Stream.add t.evs ev

let join ~channel_id t =
  let p, u = Promise.create () in
  Eio.Stream.add t.evs @@ Req_join (channel_id, Promise.resolve u);
  Promise.await_exn p

let leave t =
  Gateway.send_voice_state_update t.gw ~self_mute:t.muted ~self_deaf:t.deafened
    t.guild_id

let play ~audio t =
  let p, u = Promise.create () in
  Eio.Stream.add t.evs @@ Req_play (audio, Promise.resolve u);
  Promise.await_exn p

let stop t =
  let p, u = Promise.create () in
  Eio.Stream.add t.evs @@ Req_stop (Promise.resolve u);
  Promise.await_exn p
