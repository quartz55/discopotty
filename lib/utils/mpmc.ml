open Containers

type 'a sink = {
  mutable readers : 'a reader Ke.Fke.t;
  mutable buf : 'a Ke.Fke.t;
  mutable dropped : bool;
  clone : unit -> 'a sink;
}

and 'a reader = { mutable canceled : bool; dispatch : 'a option -> unit }

type 'a source = { mutable sinks : 'a sink Weak.t; mutable closed : bool }

let stub = Sys.opaque_identity (fun () -> ())

module Sink = struct
  type 'a t = 'a sink

  let clone t =
    if t.dropped then failwith "cannot clone dropped sink";
    let s = t.clone () in
    s.buf <- t.buf;
    s

  let rec make ?clone () =
    {
      readers = Ke.Fke.empty;
      buf = Ke.Fke.empty;
      dropped = false;
      clone = clone |> Option.get_or ~default:(fun () -> make ());
    }

  let peek t = Ke.Fke.peek t.buf

  let read_opt t =
    match Ke.Fke.pop t.buf with
    | Some (a, tl) ->
        t.buf <- tl;
        Some a
    | None -> None

  let schedule_read t ~f =
    match read_opt t with
    | Some a ->
        f (Some a);
        stub
    | None when t.dropped ->
        f None;
        stub
    | None ->
        let r = { canceled = false; dispatch = f } in
        t.readers <- Ke.Fke.push t.readers r;
        fun () -> if not r.canceled then r.canceled <- true

  let push t v =
    if t.dropped then false
    else
      let rec f q =
        match Ke.Fke.pop q with
        | Some ({ canceled = true; _ }, tl) ->
            print_endline "skipping cancelled reader";
            f tl
        | Some ({ dispatch; _ }, tl) ->
            print_endline "waking up reader";
            t.readers <- tl;
            dispatch (Some v)
        | None ->
            print_endline "pushed to buffer";
            t.readers <- Ke.Fke.empty;
            t.buf <- Ke.Fke.push t.buf v
      in
      f t.readers;
      true

  let drop t =
    if not t.dropped then (
      t.dropped <- true;
      Ke.Fke.iter
        (function
          | { canceled = true; _ } -> () | { dispatch; _ } -> dispatch None)
        t.readers;
      t.readers <- Ke.Fke.empty)

  let map ~f t =
    let o = make () in
    let rec fwd = function
      | Some v ->
          if push o (f v) then schedule_read t ~f:fwd |> ignore else drop o
      | None -> drop o
    in
    schedule_read t ~f:fwd |> ignore;
    o

  let filter ~f t =
    let o = make () in
    let rec fwd = function
      | Some v when f v ->
          if push o v then schedule_read t ~f:fwd |> ignore else drop o
      | Some _ -> schedule_read t ~f:fwd |> ignore
      | None -> drop o
    in
    schedule_read t ~f:fwd |> ignore;
    o

  let iter t ~f =
    let cancel = ref stub in
    let rec iter' = function
      | Some v ->
          f (Some v);
          cancel := schedule_read t ~f:iter'
      | None -> f None
    in
    cancel := schedule_read t ~f:iter';
    fun () -> !cancel ()

  let rec merge sinks =
    let ks = ref @@ ((1 lsl List.length sinks) - 1) in
    let o = make ~clone:(fun () -> List.map clone sinks |> merge) () in
    let c = Array.make (List.length sinks) stub in
    let cancel () = Array.iter (fun c -> c ()) c in
    let rec fwd i k = function
      | Some v ->
          if push o v then c.(i) <- schedule_read k ~f:(fwd i k)
          else (
            cancel ();
            List.iter drop sinks)
      | None ->
          ks := !ks land lnot (1 lsl i);
          if !ks = 0 then drop o
    in
    List.iteri (fun i k -> c.(i) <- schedule_read k ~f:(fwd i k)) sinks;
    o

  module Lwt = struct
    let read t =
      let p, u = Lwt.task () in
      let cancel = schedule_read t ~f:(Lwt.wakeup_later u) in
      Lwt.on_cancel p cancel;
      p

    let iter_s ~f t =
      let open Lwt.Infix in
      let rec iter () =
        read t >>= function Some v -> f v >>= iter | None -> Lwt.return_unit
      in
      iter ()

    let iter_p ~f t =
      let open Lwt.Infix in
      let rec cleanup ?(acc = []) = function
        | [] -> Ok acc
        | p :: ps -> (
            match Lwt.state p with
            | Lwt.Fail e -> Error e
            | Lwt.Sleep -> cleanup ~acc:(p :: acc) ps
            | Lwt.Return _ -> cleanup ~acc ps)
      in
      let rec join ?(acc = []) () =
        read t >>= function
        | None -> Lwt.join acc
        | Some v -> (
            match cleanup acc with
            | Ok acc -> join ~acc:(f v :: acc) ()
            | Error exn -> Lwt.fail exn)
      in
      join ()
  end
end

module Source = struct
  type 'a t = 'a source

  let sinks t =
    Seq.(0 --^ Weak.length t.sinks) |> Seq.filter_map (Weak.get t.sinks)

  let write t x =
    if t.closed then false
    else
      sinks t
      |> Seq.filter (fun s -> not @@ s.dropped)
      |> Seq.fold (fun flag s -> flag || Sink.push s x) false

  let close t =
    t.closed <- true;
    sinks t |> Seq.iter Sink.drop
end

let grow t =
  let len = Weak.length t.sinks in
  if Obj.Ephemeron.max_ephe_length - len < len then
    failwith "reached maximum number of sinks";
  let w = Weak.create (len * 2) in
  Weak.blit t.sinks 0 w 0 len;
  t.sinks <- w

let get_free_sink_ref t =
  let len = Weak.length t.sinks in
  let rec f' ?(i = 0) w =
    if i >= len then None
    else
      match Weak.get w i with
      | None -> Some i
      | Some c when c.dropped -> Some i
      | Some _ -> f' ~i:(i + 1) w
  in
  match f' t.sinks with
  | Some i -> i
  | None ->
      grow t;
      len

let create () =
  let sinks = Weak.create 4 in
  let source = { sinks; closed = false } in
  let rec make_sink () =
    let r = get_free_sink_ref source in
    let sink =
      {
        readers = Ke.Fke.empty;
        buf = Ke.Fke.empty;
        dropped = false;
        clone = make_sink;
      }
    in
    Gc.finalise
      (fun k ->
        Printf.printf "gc'ing sink with %d buffered elements and %d readers\n%!"
          (Ke.Fke.length k.buf) (Ke.Fke.length k.readers);
        Sink.drop k)
      sink;
    Weak.set source.sinks r (Some sink);
    sink
  in
  (source, make_sink ())
