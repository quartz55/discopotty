open! Disco_core.Globals
module Snowflake = Disco_models.Snowflake
module E = Disco_core.Events
module Sf_map = Map.Make (Snowflake)
module Gateway = Disco_core.Gateway
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field

type t = {
  mtx : Eio_mutex.t;
  gw : Gateway.t;
  mutable calls : Call.t Sf_map.t;
  spawn : snowflake -> Call.t;
  poison : unit -> unit;
  mutable dead : bool;
}

let user_id { gw; _ } =
  let user = Gateway.user gw in
  user.id

let server_update t srv =
  Eio_mutex.with_ t.mtx @@ fun () ->
  L.info (fun m -> m "srv update");
  Sf_map.get srv.E.Voice_server_update.guild_id t.calls
  |> Option.iter (fun call ->
         Call.update_server ~token:srv.E.Voice_server_update.token
           ~endpoint:srv.endpoint call)

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

let make ~env ~sw gw =
  let net = Eio.Stdenv.net env in
  let dmgr = Eio.Stdenv.domain_mgr env in
  let evs = Eio.Stream.create 0 in
  let poison () = Eio.Stream.add evs `Poison in
  let rec t =
    {
      mtx = Eio_mutex.make ();
      calls = Sf_map.empty;
      gw;
      spawn;
      poison;
      dead = false;
    }
  and spawn guild_id =
    let p, u = Promise.create () in
    Eio.Stream.add evs @@ `Spawn (guild_id, Promise.resolve u);
    Promise.await_exn p
  in
  Gateway.sub gw ~fn:(function
    | E.Voice_server_update v -> server_update t v
    | Voice_state_update v -> state_update t v
    | _ -> ());
  let run () =
    Switch.run @@ fun sw ->
    let rec loop () =
      match Eio.Stream.take evs with
      | `Poison ->
          Eio_mutex.with_ t.mtx @@ fun () ->
          Sf_map.to_seq t.calls
          |> Seq.map (fun (_, call) () -> Call.leave call)
          |> Seq.to_list |> Fiber.all;
          t.calls <- Sf_map.empty
      | `Spawn (guild_id, req) ->
          Eio_mutex.with_ t.mtx (fun () ->
              match Sf_map.get guild_id t.calls with
              | Some call -> req @@ Ok call
              | None ->
                  let res =
                    Result.guard @@ fun () ->
                    Call.make ~sw ~net ~dmgr ~guild_id gw
                  in
                  Result.iter
                    (fun call -> t.calls <- Sf_map.add guild_id call t.calls)
                    res;
                  req res);
          loop ()
    in
    loop ();
    raise Exit
  in
  Fiber.fork ~sw (fun () -> try run () with Exit -> ());
  t

let destroy t =
  if not @@ t.dead then (
    t.dead <- true;
    t.poison ())

let get t ~guild_id =
  Eio_mutex.lock t.mtx;
  match Sf_map.get guild_id t.calls with
  | Some call ->
      Eio_mutex.unlock t.mtx;
      call
  | None ->
      Eio_mutex.unlock t.mtx;
      t.spawn guild_id
