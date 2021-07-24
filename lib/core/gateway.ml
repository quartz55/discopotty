open! Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Payload = Gateway_payload

type t = {
  mutable user : Models.User.t;
  shards : shard list;
  id_bucket : Token_bucket.t;
  ev_rx : Events.t Lwt_pipe.Reader.t;
  ev_snk : Events.t Mpmc.Sink.t;
}

and shard = Lwt_mutex.t * Session.t

let disconnect t =
  List.map
    (fun (mtx, sess) ->
      Lwt_mutex.with_lock mtx (fun () -> Session.disconnect sess))
    t.shards
  |> Lwt.join
  >>= fun () -> Lwt_pipe.close t.ev_rx

let connect ?(max_concurrency = 1) ~token url =
  let open Lwt_result.Syntax in
  (* docs: identify requests allowed per 5 seconds *)
  let rate = 1. /. (float max_concurrency /. 5.) in
  let id_bucket = Token_bucket.make ~capacity:max_concurrency rate in
  let+ main = Session.create ~id_bucket token (Uri.of_string url) in
  let user = Session.user main in
  let shards = [ main ] in
  let ev_rx = List.map Session.events shards |> Lwt_pipe.Reader.merge_all in
  let src, snk = Mpmc.make () in
  let rec fwd () =
    Lwt_pipe.read ev_rx >>= function
    | Some v -> Mpmc.Source.write src v >>= fwd
    | None -> Mpmc.Source.close src
  in
  Lwt.async fwd;
  {
    user;
    shards = List.map (fun s -> (Lwt_mutex.create (), s)) shards;
    id_bucket;
    ev_rx;
    ev_snk = snk;
  }

let user { user; _ } = user

let shards { shards; _ } = shards

let shard ~guild_id ~f { shards; _ } =
  let len = List.length shards in
  let id = Int64.(guild_id lsr 22 |> to_int) mod len in
  let mtx, sess = List.get_at_idx_exn id shards in
  Lwt_mutex.with_lock mtx (fun () -> f sess)

let main_shard { shards; _ } = List.hd shards

let events { ev_snk; _ } = ev_snk

let send_presence_update t ?since ~afk status =
  let mtx, shard = main_shard t in
  Lwt_mutex.with_lock mtx (fun () ->
      Session.send_presence_update shard ?since ~afk status)

let send_voice_state_update t ?channel_id ?self_mute ?self_deaf guild_id =
  let f shard =
    Session.send_voice_state_update shard ?channel_id ?self_mute ?self_deaf
      guild_id
  in
  shard ~guild_id ~f t

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  let f shard =
    Session.send_guild_request_members shard ?presences ?nonce ~q guild_id
  in
  shard ~guild_id ~f t
