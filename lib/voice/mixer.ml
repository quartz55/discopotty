open! Disco_core.Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module Lf_queue = Eio_utils.Lf_queue

exception Destroyed

let spin_sleep ?(acc = 100_000L) dur =
  let b = Backoff.create () in
  let elapsed =
    let start = Mtime_clock.now () in
    fun () -> Mtime.span start (Mtime_clock.now ()) |> Mtime.Span.to_uint64_ns
  in
  (if Int64.(dur > acc) then
   let init_s = Int64.(to_float (dur - acc)) /. 1e9 in
   Unix.sleepf init_s);
  while Int64.(elapsed () < dur) do
    Backoff.once b
  done

type t = {
  reqs : req Lf_queue.t;
  mutable state : state;
  mutable silence : Audio_stream.t option;
  dead : bool Atomic.t;
  wakeup : (unit -> unit) option Atomic.t;
}

and req =
  | Now_playing of stream option res
  | Play of stream * unit res
  | Stop of unit res

and 'a res = ('a, exn) result -> unit
and state = Idle | Playing of stream
and stream = { waker : bool -> unit; rx : Audio_stream.t }

let make_silence ?n () = Audio_stream.silence_frames ?n ()

let create () =
  {
    reqs = Lf_queue.create ();
    state = Idle;
    silence = None;
    dead = Atomic.make false;
    wakeup = Atomic.make None;
  }

let wakeup t =
  let b = Backoff.create () in
  let rec loop () =
    match Atomic.get t.wakeup with
    | None -> ()
    | Some w as waker when Atomic.compare_and_set t.wakeup waker None -> w ()
    | Some _ ->
        Backoff.once b;
        loop ()
  in
  loop ()

let check t = if Atomic.get t.dead then raise Destroyed

let req t req =
  check t;
  Lf_queue.push t.reqs req;
  wakeup t

let now_playing t =
  let p, u = Promise.create () in
  req t (Now_playing (Promise.resolve u));
  Promise.await_exn p

let is_playing t = Option.is_some @@ now_playing t

let stop t =
  let p, u = Promise.create () in
  req t (Stop (Promise.resolve u));
  Promise.await_exn p

let play ~s t =
  let p, u = Promise.create () in
  req t (Play (s, Promise.resolve u));
  Promise.await_exn p

let destroy t =
  Atomic.set t.dead true;
  wakeup t

let destroy_active t =
  match t.state with
  | Playing s ->
      s.waker false;
      t.state <- Idle
  | Idle -> ()

let run ~play ~stop t =
  let now () = Mtime_clock.elapsed_ns () in
  let rec exhaust ?(i = 0) src =
    Audio_stream.read src |> function
    | Some frame ->
        let i = i + 1 in
        if play (i + 1) frame then exhaust ~i src else `Yield i
    | None -> `Closed i
  in
  let framelen = Int64.(of_int Rtp._FRAME_LEN * 100_000L) in
  let schedule_next ?drift ?(frames = 1) () =
    let timeout = Int64.(of_int frames * framelen) in
    let with_drift d =
      let delta = Int64.(now () - d) in
      Int64.(timeout - min delta timeout)
    in
    let timeout =
      Option.map with_drift drift |> Option.get_or ~default:timeout
    in
    L.trace (fun m ->
        m "sent %d frames, next tick in %Ld ns (%f s)" frames timeout
          (Int64.to_float timeout /. 1e9));
    spin_sleep timeout
  in
  let rec loop ?(drift = now ()) ?(i = 0) () =
    if Atomic.get t.dead then (
      destroy_active t;
      (* TODO @quartz55: cancel/cleanup queued requests (how?) *)
      raise Exit);
    handle_reqs ();
    process ~drift ~i
  and handle_reqs () =
    match Lf_queue.pop t.reqs with
    | Some (Play (s, res)) ->
        (match t.state with
        | Playing os ->
            os.waker false;
            if Option.is_none t.silence then t.silence <- Some (make_silence ());
            t.state <- Playing s
        | Idle -> t.state <- Playing s);
        res (Ok ());
        handle_reqs ()
    | Some (Stop res) ->
        destroy_active t;
        res (Ok ());
        handle_reqs ()
    | Some (Now_playing res) ->
        res @@ Ok (match t.state with Idle -> None | Playing s -> Some s);
        handle_reqs ()
    | None -> ()
  and process ~drift ~i =
    match (t.silence, t.state) with
    | Some s, st -> (
        exhaust ~i s |> function
        | `Closed i ->
            t.silence <- None;
            (match st with Idle -> stop () | _ -> ());
            loop ~drift ~i ()
        | `Yield frames ->
            schedule_next ~drift ~frames ();
            loop ())
    | None, Playing s -> (
        exhaust ~i s.rx |> function
        | `Closed i ->
            t.silence <- Some (make_silence ());
            t.state <- Idle;
            s.waker true;
            loop ~drift ~i ()
        | `Yield frames ->
            schedule_next ~drift ~frames ();
            loop ())
    | None, Idle ->
        assert (Option.is_none @@ Atomic.get t.wakeup);
        let p, u = Promise.create () in
        Atomic.set t.wakeup (Some (Promise.resolve u));
        Promise.await p;
        loop ()
  in
  try loop () with Exit -> ()
