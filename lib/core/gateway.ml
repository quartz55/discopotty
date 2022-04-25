open! Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module Payload = Gateway_payload
module Token_bucket = Lf_token_bucket

module Subs = struct
  type t = { state : state Atomic.t; gc : int Atomic.t }

  and state =
    | Waiting of (unit Promise.t * unit Promise.u)
    | Serving of sub Atomic.t list
    | Gcing of (unit Promise.t * unit Promise.u)

  and sub = Active of (Events.t -> unit) | Inactive

  let make () =
    { state = Atomic.make @@ Waiting (Promise.create ()); gc = Atomic.make 0 }

  let sub ~fn t =
    let sub = Atomic.make @@ Active fn in
    let unsub () =
      let b = Backoff.create () in
      let rec loop () =
        match Atomic.get sub with
        | Active _ as st when Atomic.compare_and_set sub st Inactive ->
            Atomic.incr t.gc
        | Active _ ->
            Backoff.once b;
            loop ()
        | Inactive -> ()
      in
      loop ()
    in
    let b = Backoff.create () in
    let rec loop () =
      match Atomic.get t.state with
      | Waiting (_, u) as st
        when Atomic.compare_and_set t.state st (Serving [ sub ]) ->
          Promise.resolve u ();
          unsub
      | Serving s as st when Atomic.get t.gc >= 5 ->
          let p, u = Promise.create () in
          if Atomic.compare_and_set t.state st (Gcing (p, u)) then (
            let gced = ref 0 in
            let s =
              List.filter
                (fun s ->
                  match Atomic.get s with
                  | Active _ -> true
                  | Inactive ->
                      incr gced;
                      Atomic.decr t.gc;
                      false)
                s
            in
            L.dbg (fun m -> m "gced %d inactive subs" !gced);
            Atomic.set t.state (Serving (sub :: s));
            Promise.resolve u ();
            unsub)
          else (
            Backoff.once b;
            loop ())
      | Serving s as st
        when Atomic.compare_and_set t.state st (Serving (sub :: s)) ->
          unsub
      | Gcing (p, _) ->
          Promise.await p;
          loop ()
      | Waiting _ | Serving _ ->
          Backoff.once b;
          loop ()
    in
    loop ()

  let rec pub ~sw t v =
    match Atomic.get t.state with
    | Waiting (p, _) | Gcing (p, _) ->
        Promise.await p;
        pub ~sw t v
    | Serving s ->
        List.iter
          (fun s ->
            match Atomic.get s with
            | Active fn -> Fiber.fork ~sw (fun () -> fn v)
            | Inactive -> ())
          s
end

type t = {
  mutable user : Models.User.t;
  shards : shard list;
  subs : Subs.t;
  dead : unit Promise.t * unit Promise.u;
}

and shard = Eio_mutex.t * Session.t

let check { dead; _ } =
  if Promise.is_resolved (fst dead) then
    raise @@ Invalid_argument "dead gateway"

let disconnect t =
  if not @@ Promise.is_resolved (fst t.dead) then
    Promise.resolve (snd t.dead) ()

let connect ~sw ~net ~dmgr:_ ?(max_concurrency = 1) ~token url =
  assert (max_concurrency > 0);
  let p, u = Promise.create () in
  let spawn () : no_return =
    Switch.run @@ fun sw ->
    (* docs: identify requests allowed per 5 seconds *)
    let rate = 1. /. (float max_concurrency /. 5.) in
    let id_bucket = Token_bucket.make ~capacity:max_concurrency rate in
    let subs = Subs.make () in
    let fwd = Subs.pub ~sw subs in
    let main =
      Session.create ~sw ~net ~id_bucket ~fwd token (Uri.of_string url)
    in
    let user = Session.user main in
    (* TODO @quartz55: multiple shards using domains *)
    (* TODO @quartz55: max_concurrency "bucket" handling *)
    let shards = [ main ] in
    let t =
      {
        user;
        subs;
        shards = List.map (fun s -> (Eio_mutex.make (), s)) shards;
        dead = Promise.create ();
      }
    in
    Promise.resolve u t;
    Promise.await (fst t.dead);
    t.shards
    |> List.map (fun (mtx, sess) () ->
           Eio_mutex.with_ mtx @@ fun () -> Session.disconnect sess)
    |> Fiber.all;
    raise Exit
  in
  Fiber.fork ~sw (fun () -> match spawn () with _ -> . | exception Exit -> ());
  Promise.await p

let user { user; _ } = user
let shards { shards; _ } = shards

let subscribe ~fn t =
  check t;
  Subs.sub ~fn t.subs

let sub = subscribe

let shard ~guild_id ~f { shards; _ } =
  let len = List.length shards in
  let id = Int64.(guild_id lsr 22 |> to_int) mod len in
  let mtx, sess = List.get_at_idx_exn id shards in
  Eio_mutex.with_ mtx @@ fun () -> f sess

let main_shard { shards; _ } = List.hd shards

let send_presence_update t ?since ~afk status =
  check t;
  let mtx, shard = main_shard t in
  Eio_mutex.with_ mtx @@ fun () ->
  Session.send_presence_update shard ?since ~afk status

let send_voice_state_update t ?channel_id ?self_mute ?self_deaf guild_id =
  check t;
  let f shard =
    Session.send_voice_state_update shard ?channel_id ?self_mute ?self_deaf
      guild_id
  in
  shard ~guild_id ~f t

let send_guild_request_members t ?presences ?nonce ~q guild_id =
  check t;
  let f shard =
    Session.send_guild_request_members shard ?presences ?nonce ~q guild_id
  in
  shard ~guild_id ~f t
