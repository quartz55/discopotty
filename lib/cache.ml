open Lwt.Infix

type t = Cache

let create () = Lwt.return Cache
