(* open! Disco_core.Globals
   module Snowflake = Disco_models.Snowflake
   module E = Disco_core.Events
   module Sf_map = Map.Make (Snowflake)
   module Gateway = Disco_core.Gateway
   module L = (val Relog.logger ~namespace:__MODULE__ ())
   module F = Relog.Field

   type t = { mtx : Eio_mutex.t; gw : Gateway.t; mutable calls : Call.t Sf_map.t }

   let user_id { gw; _ } =
     let user = Gateway.user gw in
     user.id

   let server_update t srv =
     Eio_mutex.with_ t.mtx @@ fun () ->
     L.info (fun m -> m "srv update");
     Sf_map.get srv.E.Voice_server_update.guild_id t.calls
     |> Option.iter (fun call ->
            Call.update_server ~token:srv.token ~endpoint:srv.endpoint call)

   let state_update t vst =
     Eio_mutex.with_ t.mtx @@ fun () ->
     L.info (fun m -> m "state update");
     match vst.E.Voice_state.guild_id with
     | Some guild_id when Snowflake.(user_id t = vst.user_id) ->
         Sf_map.get guild_id t.calls
         |> Option.iter (fun call ->
                Call.update_session ?channel_id:vst.channel_id
                  ~session_id:vst.session_id call)
     | _ -> ()

   let make gw =
     let t = { mtx = Eio_mutex.make (); calls = Sf_map.empty; gw } in
     Gateway.sub gw ~fn:(function
       | E.Voice_server_update v -> server_update t v
       | Voice_state_update v -> state_update t
       | _ -> ());
     t

   let destroy t =
     Eio_mutex.with_ t.mtx @@ fun () ->
     Sf_map.to_seq t.calls
     |> Seq.map (fun (_, call) () -> Call.leave call)
     |> Seq.to_list |> Fiber.all;
     t.calls <- Sf_map.empty

   let get t ~guild_id =
     Eio_mutex.with_ t.mtx @@ fun () ->
     match Sf_map.get guild_id t.calls with
     | Some call -> call
     | None ->
         let call = Call.make t.gw ~guild_id |> Result.get_exn in
         t.calls <- Sf_map.add guild_id call t.calls;
         call *)
