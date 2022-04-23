open Containers
module L = (val Relog.logger ~namespace:__MODULE__ ())

(* author's note: decided to go with unboxed ints
   in the end, I don't expect this to be used in systems
   other than 64 bit (and 63 bits is more than enough) *)
type t = {
  capacity : int;
  capacity_ns : int;
  ns_per_token : int;
  mutable tokens : int;
  mutable ts : Mtime.Span.t;
  mutable waiters : waiter Ke.Fke.t;
}

and waiter = Waiter of int * unit Lwt.u * unit Lwt.t

let check_capacity ~n { capacity; _ } =
  if n > capacity then
    raise (Invalid_argument "can't take more than bucket's capacity")

let make ~capacity ?init rate =
  let ns_per_token = Int.of_float (1e9 /. rate |> ceil) in
  let init = match init with Some i -> min capacity i | None -> capacity in
  {
    capacity;
    capacity_ns = capacity * ns_per_token;
    ns_per_token;
    tokens = init * ns_per_token;
    ts = Mtime_clock.elapsed ();
    waiters = Ke.Fke.empty;
  }

let capacity { capacity; _ } = capacity
let rate { ns_per_token; _ } = 1. /. (float ns_per_token /. 1e9)
let is_full { capacity_ns; tokens; _ } = tokens >= capacity_ns

let fill t =
  if is_full t then t
  else
    let now = Mtime_clock.elapsed () in
    let span = Mtime.Span.abs_diff now t.ts in
    let diff_ns = Mtime.Span.to_uint64_ns span |> Int64.to_int in
    t.tokens <- min t.capacity_ns (t.tokens + diff_ns);
    t.ts <- now;
    t

let try_take_ns_unsafe ~n t =
  let take' t =
    if t.tokens >= n then (
      t.tokens <- t.tokens - n;
      true)
    else false
  in
  if take' t then true else fill t |> take'

let try_take ?(n = 1) t =
  check_capacity ~n t;
  if Ke.Fke.is_empty t.waiters then
    let n_ns = n * t.ns_per_token in
    try_take_ns_unsafe ~n:n_ns t
  else false

let attend_waiters t =
  let rec next' () =
    match Ke.Fke.pop t.waiters with
    | Some (Waiter (n, u, _), ke) ->
        if try_take_ns_unsafe ~n t then (
          t.waiters <- ke;
          Lwt.wakeup_later u ();
          next' ())
        else
          let wait_ns = max (n - t.tokens) 0 in
          let wait_secs = float wait_ns /. 1e9 in
          Lwt.bind (Lwt_unix.sleep wait_secs) next'
    | None -> Lwt.return ()
  in
  Lwt.async next'

let take ?(n = 1) t =
  check_capacity ~n t;
  let n_ns = n * t.ns_per_token in
  let enqueue' () =
    let p, u = Lwt.task () in
    t.waiters <- Ke.Fke.push t.waiters (Waiter (n_ns, u, p));
    p
  in
  match Ke.Fke.is_empty t.waiters with
  | true ->
      if try_take_ns_unsafe ~n:n_ns t then Lwt.return ()
      else
        let p = enqueue' () in
        attend_waiters t;
        p
  | false -> enqueue' ()

let take_then ~f ?n t = take ?n t |> Lwt.map (fun () -> f ())
let wrap_take ~f t ?(n = 1) () = Lwt.bind (take ~n t) (fun () -> f)

let cancel_waiting t =
  let rec f' ke =
    match Ke.Fke.pop ke with
    | Some (Waiter (_, _, p), ke) ->
        Lwt.cancel p;
        f' ke
    | None -> ()
  in
  f' t.waiters;
  t.waiters <- Ke.Fke.empty
