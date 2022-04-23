type t = Eio.Semaphore.t

let make () = Eio.Semaphore.make 1
let lock t = Eio.Semaphore.acquire t
let is_locked t = Eio.Semaphore.get_value t = 0

let unlock t =
  if not @@ is_locked t then raise @@ Invalid_argument "already unlocked";
  Eio.Semaphore.release t

let with_ t fn =
  lock t;
  Fun.protect ~finally:(fun () -> unlock t) fn

let with_lock = with_
