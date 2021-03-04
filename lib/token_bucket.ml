open Containers

module L = (val Relog.logger ~namespace:__MODULE__ ())

type t = {
  capacity : int;
  capacity_ns : int64;
  ns_per_token : int64;
  mutable tokens : int64;
  mutable ts : Mtime.Span.t;
  waiters : waiter CCDeque.t;
}

and waiter = Waiter of int64 * unit Lwt.u

let check_capacity ~n { capacity; _ } =
  if n > capacity then
    raise (Invalid_argument "can't take more than bucket's capacity")

let make ~capacity ?init rate =
  let ns_per_token = Int64.of_float (1e9 /. rate |> ceil) in
  let init = match init with Some i -> min capacity i | None -> capacity in
  {
    capacity;
    capacity_ns = Int64.(of_int capacity * ns_per_token);
    ns_per_token;
    tokens = Int64.(of_int init * ns_per_token);
    ts = Mtime_clock.elapsed ();
    waiters = CCDeque.create ();
  }

let capacity { capacity; _ } = capacity

let rate { ns_per_token; _ } = Int64.to_float ns_per_token /. 1e9

let is_full { capacity_ns; tokens; _ } = Int64.(tokens >= capacity_ns)

let fill t =
  if is_full t then t
  else
    let now = Mtime_clock.elapsed () in
    let span = Mtime.Span.abs_diff now t.ts in
    let diff_ns = Mtime.Span.to_uint64_ns span in
    t.tokens <- Int64.(min t.capacity_ns (t.tokens + diff_ns));
    t.ts <- now;
    t

let try_take_ns_unsafe ~n t =
  let take' t =
    if Int64.(t.tokens >= n) then (
      t.tokens <- Int64.(t.tokens - n);
      true)
    else false
  in
  if take' t then true else fill t |> take'

let try_take ?(n = 1) t =
  check_capacity ~n t;
  if CCDeque.is_empty t.waiters then
    let n_ns = Int64.(of_int n * t.ns_per_token) in
    try_take_ns_unsafe ~n:n_ns t
  else false

let attend_waiters t =
  let rec next' () =
    match CCDeque.peek_front_opt t.waiters with
    | Some (Waiter (n, u)) ->
        if try_take_ns_unsafe ~n t then (
          CCDeque.remove_front t.waiters;
          Lwt.wakeup_later u ();
          next' ())
        else
          let wait_ns = Int64.(max (n - t.tokens) 0L) in
          let wait_secs = Int64.to_float wait_ns /. 1e9 in
          Lwt.bind (Lwt_unix.sleep wait_secs) next'
    | None -> Lwt.return ()
  in
  Lwt.async next'

let take ?(n = 1) t =
  check_capacity ~n t;
  let n_ns = Int64.(of_int n * t.ns_per_token) in
  let enqueue' () =
    let p, u = Lwt.task () in
    CCDeque.push_back t.waiters (Waiter (n_ns, u));
    p
  in
  match CCDeque.is_empty t.waiters with
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
  let rec f' () =
    match CCDeque.take_front_opt t.waiters with
    | Some (Waiter (_, u)) ->
        Lwt.wakeup_later_exn u Lwt.Canceled;
        f' ()
    | None -> ()
  in
  f' ()
