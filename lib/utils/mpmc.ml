open! Containers
open Lwt.Infix

module MakeCache (T : Ephemeron.SeededS) = struct
  module Table = T

  type key = Table.key

  type 'a t = 'a Table.t * Lwt_mutex.t

  let make () = (Table.create ~random:true 32, Lwt_mutex.create ())

  let access (table, lock) f =
    Lwt_mutex.with_lock lock @@ fun () -> table |> f |> Lwt.return

  let add t ~key value = access t @@ fun table -> Table.add table key value

  let find t ~key = access t @@ fun table -> Table.find table key

  let find_opt t ~key = access t @@ fun table -> Table.find_opt table key

  let iter t ~f = access t @@ fun table -> Table.iter f table

  let mem t ~key = access t @@ fun table -> Table.mem table key

  let remove t ~key = access t @@ fun table -> Table.remove table key

  let reset t = access t Table.reset

  let clean t = access t Table.clean

  let of_seq seq =
    let tbl, mtx = make () in
    Seq.iter (fun (k, v) -> Table.add tbl k v) seq;
    (tbl, mtx)
end

type key = K of int

module Cache = MakeCache (Ephemeron.K1.MakeSeeded (struct
  let equal (K l) (K r) = l = r

  let hash = Hashtbl.seeded_hash

  type t = key
end))

type 'a source = { sinks : 'a sink Cache.t; mutable closed : bool }

and 'a sink = {
  mutable readers : 'a reader Ke.Fke.t;
  mutable buf : 'a Ke.Fke.t;
  mutable dropped : bool;
}

and 'a reader = 'a option Lwt.t * 'a option Lwt.u

type 'a handle = { key : Cache.key; source : 'a source }

let gen_key () =
  Random.self_init ();
  K (Random.bits ())

let make_sink () =
  { readers = Ke.Fke.empty; buf = Ke.Fke.empty; dropped = false }

let connect source sink =
  let sink = { sink with dropped = source.closed } in
  let key = gen_key () in
  Cache.add source.sinks ~key sink >|= fun () -> { key; source }

let drop_sink t =
  if not t.dropped then (
    t.dropped <- true;
    Ke.Fke.iter
      (fun (p, u) -> if Lwt.is_sleeping p then Lwt.wakeup_later u None)
      t.readers;
    t.readers <- Ke.Fke.empty)

module Sink = struct
  type 'a t = 'a handle

  let get { key; source } = Cache.find source.sinks ~key

  let peek t = get t >|= fun t -> Ke.Fke.peek t.buf

  let pull t =
    get t >>= fun t ->
    match Ke.Fke.pop t.buf with
    | Some (v, tl) ->
        t.buf <- tl;
        Lwt.return @@ Some v
    | None when t.dropped -> Lwt.return None
    | None ->
        let p, u = Lwt.task () in
        t.readers <- Ke.Fke.push t.readers (p, u);
        p

  let drop t = get t >|= drop_sink

  let clone t = get t >>= fun sink -> connect t.source sink
end

module Source = struct
  type 'a t = 'a source

  let make ?(sinks = Cache.make ()) () = { sinks; closed = false }

  let subscribe source = make_sink () |> connect source

  let write source v =
    let f _ sink =
      (* Printf.printf "handle %d: " k; *)
      if sink.dropped then ()
      else
        let rec f q =
          match Ke.Fke.pop q with
          | Some ((p, u), tl) when Lwt.is_sleeping p ->
              (* print_endline "waking up reader"; *)
              sink.readers <- tl;
              Lwt.wakeup_later u @@ Some v
          | Some (_, tl) ->
              (* print_endline "skipping cancelled reader"; *)
              f tl
          | None ->
              (* print_endline "pushed to buffer"; *)
              sink.readers <- Ke.Fke.empty;
              sink.buf <- Ke.Fke.push sink.buf v
        in
        f sink.readers
    in
    Cache.iter source.sinks ~f

  let close source =
    if source.closed then Lwt.return_unit
    else Cache.iter source.sinks ~f:(fun _ -> drop_sink)
end

let make () =
  let sink = make_sink () in
  let key = gen_key () in
  let sinks = Seq.pure (key, sink) |> Cache.of_seq in
  let source = Source.make ~sinks () in
  (source, { key; source })
