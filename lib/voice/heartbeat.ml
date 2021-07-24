open! Disco_core.Globals

type t = {
  mutable interval : float;
  mutable preempt : ?interval:float -> unit -> unit;
  mutable ack : int -> unit;
  mutable cancel : unit -> unit;
}

let interval { interval; _ } = interval

let preempt ?interval { preempt; _ } = preempt ?interval ()

let ack nonce { ack; _ } = ack nonce

let cancel { cancel; _ } = cancel ()

let make ?err fn interval =
  let err =
    Option.get_or
      ~default:(fun () -> failwith "no ACK of last heartbeat received")
      err
  in
  let out =
    {
      interval;
      preempt = (fun ?interval:_ -> ignore);
      ack = ignore;
      cancel = ignore;
    }
  in
  let gen_nonce =
    let st = Random.State.make_self_init () in
    fun () -> Random.State.bits st
  in
  let rec loop () =
    let open Lwt.Syntax in
    let acked = ref false in
    let nonce = gen_nonce () in
    let () = fn nonce in
    let p_preempt, u_preempt = Lwt.wait () in
    let p_sleep = Lwt_unix.sleep out.interval |> Lwt.map (Fun.const `Sleep) in
    out.ack <- (fun n -> if not !acked then acked := nonce = n);
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