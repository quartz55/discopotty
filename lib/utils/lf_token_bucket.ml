open Containers
open Eio.Std
module L = (val Relog.logger ~namespace:__MODULE__ ())
module Lf_queue = Eio_utils.Lf_queue

(* author's note: decided to go with unboxed ints
   in the end, I don't expect this to be used in systems
   other than 64 bit (and 63 bits is more than enough) *)
type t = {
  sleep : float -> unit;
  capacity : int;
  capacity_ns : int;
  ns_per_token : int;
  bucket : bucket Atomic.t;
  attending : bool Atomic.t;
  waiters : waiter Lf_queue.t;
}

and bucket = { tokens : int; ts : Mtime.Span.t }
and waiter = Waiter of int * unit Promise.u * unit Promise.t

let check_capacity ~n { capacity; _ } =
  if n > capacity then
    raise (Invalid_argument "can't take more than bucket's capacity")

let make ~capacity ?init rate =
  let sleep secs = Eio_unix.sleep secs in
  let ns_per_token = Int.of_float (1e9 /. rate |> ceil) in
  let init = match init with Some i -> min capacity i | None -> capacity in
  let bucket = { tokens = init * ns_per_token; ts = Mtime_clock.elapsed () } in
  {
    sleep;
    capacity;
    capacity_ns = capacity * ns_per_token;
    ns_per_token;
    bucket = Atomic.make bucket;
    attending = Atomic.make false;
    waiters = Lf_queue.create ();
  }

let capacity { capacity; _ } = capacity
let rate { ns_per_token; _ } = 1. /. (float ns_per_token /. 1e9)

let is_full { capacity_ns; bucket; _ } =
  let { tokens; _ } = Atomic.get bucket in
  tokens >= capacity_ns

let fill t =
  let b = Backoff.create () in
  let rec loop () =
    let bucket = Atomic.get t.bucket in
    match bucket with
    | { tokens; _ } when tokens >= t.capacity_ns -> t
    | { tokens; ts } ->
        let now = Mtime_clock.elapsed () in
        let span = Mtime.Span.abs_diff now ts in
        let diff_ns = Mtime.Span.to_uint64_ns span |> Int64.to_int in
        let tokens = min t.capacity_ns (tokens + diff_ns) in
        if Atomic.compare_and_set t.bucket bucket { tokens; ts = now } then t
        else (
          Backoff.once b;
          loop ())
  in
  loop ()

let try_take_ns_unsafe ~n t =
  let b = Backoff.create () in
  let rec take' t =
    let ({ tokens; _ } as bucket) = Atomic.get t.bucket in
    let cas tokens =
      Atomic.compare_and_set t.bucket bucket { bucket with tokens }
    in
    match tokens - n with
    | n when n < 0 -> false
    | n when cas n -> true
    | _ ->
        Backoff.once b;
        take' t
  in
  if take' t then true else fill t |> take'

let try_take ?(n = 1) t =
  check_capacity ~n t;
  if Atomic.compare_and_set t.attending false true then (
    let n_ns = n * t.ns_per_token in
    let out = try_take_ns_unsafe ~n:n_ns t in
    Atomic.set t.attending false;
    out)
  else false

let attend_waiters t =
  let b = Backoff.create () in
  let rec attend ?(n = 0) () =
    traceln "Attending...";
    match Lf_queue.pop t.waiters with
    | None when n = 0 ->
        Backoff.once b;
        attend ()
    | None -> Atomic.set t.attending false
    | Some (Waiter (n, u, _)) when try_take_ns_unsafe ~n t ->
        Promise.resolve u ();
        attend ~n:(n + 1) ()
    | Some (Waiter (n, _, _) as waiter) ->
        Lf_queue.push_head t.waiters waiter;
        let { tokens; _ } = Atomic.get t.bucket in
        let wait_ns = max (n - tokens) 0 in
        let wait_secs = float wait_ns /. 1e9 in
        t.sleep wait_secs;
        attend ~n ()
  in
  attend ~n:0

let take ~sw ?(n = 1) t =
  check_capacity ~n t;
  let n_ns = n * t.ns_per_token in
  let enqueue' () =
    let p, u = Promise.create () in
    Lf_queue.push t.waiters (Waiter (n_ns, u, p));
    p
  in
  match Atomic.compare_and_set t.attending false true with
  | false -> Promise.await @@ enqueue' ()
  | true when try_take_ns_unsafe ~n:n_ns t -> Atomic.set t.attending false
  | true ->
      let p = enqueue' () in
      Fiber.fork ~sw @@ attend_waiters t;
      Promise.await p
