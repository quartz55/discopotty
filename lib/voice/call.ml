(* open! Disco_core.Globals
   open Lwt.Infix
   module Snowflake = Disco_models.Snowflake
   module E = Disco_core.Events
   module Gateway = Disco_core.Gateway
   module F = Relog.Field

   module L = (val Relog.logger ~namespace:"Disco_voice__Call" ())

   type 'a req = ('a, Error.t) result Lwt.t * ('a, Error.t) result Lwt.u

   type srv_info = { token : string; endpoint : string }

   type sess_info = { channel_id : snowflake; session_id : string }

   type t = {
     gw : Gateway.t;
     mixer : Mixer.t;
     guild_id : snowflake;
     mutable muted : bool;
     mutable deafened : bool;
     tx : op Lwt_pipe.Writer.t;
   }

   and op = Req of op_req | Gw of gw_update

   and op_req = Join of snowflake * unit req

   and gw_update = Srv of srv_info | Sess of sess_info | Dc

   type conn =
     | Detached
     | Init of { channel_id : snowflake; req : unit req; info : conn_info }
     | Connecting of { channel_id : snowflake; req : unit req }
     | Live of Session.t

   and conn_info = Empty | Got_srv of srv_info | Got_sess of sess_info

   let make ?(muted = false) ?(deafened = false) ~guild_id gw =
     let ( let+ ) = Result.( let+ ) in
     let op_rx = Lwt_pipe.create () in
     let mixer_rx = Lwt_pipe.create () in
     let+ mixer = Mixer.create (Lwt_pipe.write mixer_rx) in
     let t = { gw; guild_id; muted; deafened; tx = op_rx; mixer } in

     let read_op () =
       Lwt_pipe.read op_rx >|= Option.get_exn >|= function
       | Req r -> `Op r
       | Gw g -> `Gw g
     in
     let _read_mixer () =
       Lwt_pipe.read mixer_rx >|= Option.get_exn >|= fun mix -> `Mixer mix
     in

     let do_connect ~srv ~sess =
       let user_id = (Gateway.user t.gw).id in
       let guild_id = t.guild_id in
       Session.create ~guild_id ~user_id ~channel_id:sess.channel_id
         ~session_id:sess.session_id ~token:srv.token srv.endpoint
     in

     let run () =
       let conn = ref Detached in
       let rec poll q =
         Lwt.nchoose_split q >>= function
         | [], [] -> Lwt_result.return ()
         | rs, ps -> (
             match handle ps rs with
             | Ok [] -> Lwt_result.return ()
             | Ok q -> poll q
             | Error _ as err -> Lwt.return err)
       and handle out rs =
         match (rs, !conn) with
         | [], _ -> Ok out
         | `Noop :: xs, _ -> handle out xs
         | `Cancel :: xs, Init { req; _ } ->
             Lwt.wakeup_later (snd req) @@ Error.msg "timed out";
             conn := Detached;
             handle out xs
         | `Cancel :: xs, _ -> handle out xs
         | `Connected (Ok session) :: xs, (Init { req; _ } | Connecting { req; _ })
           ->
             conn := Live session;
             L.info (fun m -> m "connected");
             Lwt.wakeup_later (snd req) (Ok ());
             handle out xs
         | ( `Connected (Error _ as err) :: xs,
             (Init { req; _ } | Connecting { req; _ }) ) ->
             conn := Detached;
             L.err (fun m -> m "error connecting");
             Lwt.wakeup_later (snd req) err;
             handle out xs
         | `Connected (Ok session) :: xs, _ ->
             conn := Live session;
             handle out xs
         | `Connected (Error _) :: xs, _ ->
             conn := Detached;
             handle out xs
         | ( (`Op (Join (cid, nreq)) as op) :: xs,
             (Init { req; channel_id; _ } | Connecting { req; channel_id; _ }) ) ->
             L.info (fun m -> m "join request while connecting");
             if Snowflake.(cid = channel_id) then (
               Lwt.async (fun () ->
                   fst req >|= fun res -> Lwt.wakeup_later (snd nreq) res);
               handle (read_op () :: out) xs)
             else handle (Lwt.return op :: read_op () :: out) xs
         | `Op (Join (channel_id, req)) :: xs, Live session ->
             L.info (fun m -> m "join request while connected");
             if Snowflake.(channel_id = Session.channel_id session) then (
               Lwt.wakeup_later (snd req) (Ok ());
               handle (read_op () :: out) xs)
             else (
               conn := Init { channel_id; req; info = Empty };
               let reconn =
                 Session.disconnect session >>= fun () ->
                 Gateway.send_voice_state_update t.gw ~channel_id
                   ~self_mute:t.muted ~self_deaf:t.deafened t.guild_id
                 >|= fun () -> `Noop
               in
               handle (reconn :: read_op () :: out) xs)
         | `Op (Join (channel_id, req)) :: xs, Detached ->
             L.info (fun m -> m "join request while detached");
             conn := Init { channel_id; req; info = Empty };
             let send =
               Gateway.send_voice_state_update t.gw ~channel_id ~self_mute:t.muted
                 ~self_deaf:t.deafened t.guild_id
               >|= fun () -> `Noop
             in
             let timeout =
               Lwt.pick
                 [
                   (Lwt_unix.sleep 5. >|= fun () -> `Cancel);
                   (fst req >|= fun _ -> `Noop);
                 ]
             in
             handle (send :: timeout :: read_op () :: out) xs
         | `Gw Dc :: xs, Live session ->
             L.info (fun m -> m "disconnected");
             conn := Detached;
             let dc = Session.disconnect session >|= fun () -> `Noop in
             handle (dc :: read_op () :: out) xs
         | `Gw (Srv srv) :: xs, Live s ->
             if
               String.(
                 Session.endpoint s <> srv.endpoint || Session.token s <> srv.token)
             then (
               L.warn (fun m -> m "new voice server info, reconnecting");
               let req = Lwt.wait () in
               let sess =
                 {
                   session_id = Session.session_id s;
                   channel_id = Session.channel_id s;
                 }
               in
               conn := Connecting { channel_id = sess.channel_id; req };
               let reconn =
                 Session.disconnect s >>= fun () ->
                 do_connect ~srv ~sess >|= fun res -> `Connected res
               in
               handle (reconn :: read_op () :: out) xs)
             else handle (read_op () :: out) xs
         | `Gw (Sess sess) :: xs, Live s ->
             if
               Snowflake.(Session.channel_id s <> sess.channel_id)
               || String.(Session.session_id s <> sess.session_id)
             then (
               L.warn (fun m -> m "new voice session info, reconnecting");
               let req = Lwt.wait () in
               let srv =
                 { token = Session.token s; endpoint = Session.endpoint s }
               in
               conn := Connecting { channel_id = sess.channel_id; req };
               let reconn =
                 Session.disconnect s >>= fun () ->
                 do_connect ~srv ~sess >|= fun res -> `Connected res
               in
               handle (reconn :: read_op () :: out) xs)
             else handle (read_op () :: out) xs
         | `Gw _ :: xs, Detached ->
             L.warn (fun m -> m "gateway event on detached call");
             handle (read_op () :: out) xs
         | (`Gw _ as ev) :: xs, Connecting _ ->
             L.info (fun m -> m "buffering gw event while connecting");
             handle (Lwt.return ev :: read_op () :: out) xs
         | `Gw (Srv srv) :: xs, Init { info = Got_sess sess; channel_id; req }
         | `Gw (Sess sess) :: xs, Init { info = Got_srv srv; channel_id; req } ->
             conn := Connecting { channel_id; req };
             let connect = do_connect ~srv ~sess >|= fun res -> `Connected res in
             handle (connect :: read_op () :: out) xs
         | `Gw (Srv nsrv) :: xs, Init ({ info = Got_srv _ | Empty; _ } as init) ->
             conn := Init { init with info = Got_srv nsrv };
             handle (read_op () :: out) xs
         | `Gw (Sess nsess) :: xs, Init ({ info = Got_sess _ | Empty; _ } as init)
           ->
             conn := Init { init with info = Got_sess nsess };
             handle (read_op () :: out) xs
         | `Gw Dc :: xs, Init { req; _ } ->
             Lwt.wakeup_later (snd req) (Error.msg "dc'ed when connecting?");
             handle (read_op () :: out) xs
       in
       poll [ read_op () ]
     in
     let evloop =
       run () >|= function
       | Ok () -> ()
       | Error e -> L.err (fun m -> m "call evloop crashed: %a" Error.pp e)
     in
     Lwt_pipe.keep op_rx evloop;
     Lwt.on_termination evloop (fun () ->
         Lwt_pipe.close_nonblock op_rx;
         Mixer.destroy mixer;
         Lwt_pipe.close_nonblock mixer_rx);
     t

   let update_server ~token ~endpoint t =
     Lwt_pipe.write_exn t.tx @@ Gw (Srv { token; endpoint })

   let update_session ?channel_id ~session_id t =
     let upd =
       match channel_id with
       | None -> Dc
       | Some channel_id -> Sess { channel_id; session_id }
     in
     Lwt_pipe.write_exn t.tx @@ Gw upd

   let join ~channel_id t =
     let req = Lwt.wait () in
     Lwt_pipe.write_exn t.tx @@ Req (Join (channel_id, req)) >>= fun () -> fst req

   let leave t =
     Lwt_pipe.write t.tx @@ Gw Dc >>= fun _ ->
     Gateway.send_voice_state_update t.gw ~self_mute:t.muted ~self_deaf:t.deafened
       t.guild_id *)
