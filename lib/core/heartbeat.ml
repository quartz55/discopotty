open! Globals

type t = {
  mutable interval : float;
  mutable preempt : ?interval:float -> unit -> unit;
  mutable ack : unit -> unit;
  mutable cancel : unit -> unit;
}

let interval { interval; _ } = interval

let preempt ?interval { preempt; _ } = preempt ?interval ()

let ack { ack; _ } = ack ()

let cancel { cancel; _ } = cancel ()

let make ?err fn interval =
  let err =
    Option.get_or
      ~default:(fun () -> failwith "no ACK of last heartbeat received")
      err
  in
  let stub = Fun.const () in
  let out =
    { interval; preempt = (fun ?interval:_ -> stub); ack = stub; cancel = stub }
  in
  let rec loop () =
    let open Lwt.Syntax in
    let acked = ref false in
    let () = fn () in
    let p_preempt, u_preempt = Lwt.wait () in
    let p_sleep = Lwt_unix.sleep out.interval |> Lwt.map (Fun.const `Sleep) in
    out.ack <- (fun () -> acked := true);
    out.cancel <-
      (fun () ->
        if Lwt.is_sleeping p_preempt then Lwt.wakeup_later u_preempt `Cancel);
    out.preempt <-
      (fun ?(interval = interval) () ->
        if Lwt.is_sleeping p_preempt then
          Lwt.wakeup_later u_preempt (`Preempt interval));
    let* r = Lwt.pick [ p_sleep; p_preempt ] in
    match (r, !acked) with
    | `Sleep, true -> loop ()
    | `Preempt interval, _ ->
        out.interval <- interval;
        loop ()
    | `Sleep, false ->
        err ();
        Lwt.return ()
    | `Cancel, _ -> Lwt.return ()
  in
  Lwt.async loop;
  out