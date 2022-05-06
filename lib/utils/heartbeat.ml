open Containers
open Eio.Std

module type T = sig
  type t

  val gen : unit -> t
  val eq : t -> t -> bool
end

module Unit = struct
  type t = unit

  let gen () = ()
  let eq _ _ = true
end

module Int = struct
  type t = int

  let gen =
    let st = Random.State.make_self_init () in
    fun () -> Random.State.bits st

  let eq = Int.equal
end

type 'a t = {
  mutable interval : float;
  mutable preempt : ?interval:float -> unit -> unit;
  mutable ack : 'a -> unit;
  mutable cancel : unit -> unit;
}

let interval { interval; _ } = interval
let preempt ?interval { preempt; _ } = preempt ?interval ()
let ack { ack; _ } = ack
let cancel { cancel; _ } = cancel ()

exception Missed_ACK

let make (type a) ~sw ?err ~(t : (module T with type t = a)) (fn : a -> unit)
    interval : a t =
  (* TODO @quartz55: feels like the wrong way to do this *)
  let module T = (val t) in
  let module T : T with type t := T.t = T in
  let err = Option.get_or ~default:(fun () -> raise Missed_ACK) err in
  let stub _ = () in
  let out =
    { interval; preempt = (fun ?interval:_ -> stub); ack = stub; cancel = stub }
  in
  let rec loop () =
    let acked = ref false in
    let nonce = T.gen () in
    let () = fn nonce in
    let p_preempt, u_preempt = Promise.create () in
    let p_sleep () =
      Eio_unix.sleep out.interval;
      `Sleep
    in
    out.ack <- (fun n -> if not !acked then acked := T.eq nonce n);
    out.cancel <-
      (fun () ->
        if not @@ Promise.is_resolved p_preempt then
          Promise.resolve u_preempt `Cancel);
    out.preempt <-
      (fun ?(interval = interval) () ->
        if not @@ Promise.is_resolved p_preempt then
          Promise.resolve u_preempt (`Preempt interval));
    let r = Fiber.first p_sleep (fun () -> Promise.await p_preempt) in
    match (r, !acked) with
    | `Sleep, true -> loop ()
    | `Preempt interval, _ ->
        out.interval <- interval;
        loop ()
    | `Sleep, false -> err ()
    | `Cancel, _ -> ()
  in
  Fiber.fork ~sw loop;
  out
