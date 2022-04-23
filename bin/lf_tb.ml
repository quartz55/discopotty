open Disco_utils
open Eio.Std

let consumer ~sw ~clock ?(prefix = "") bucket =
  let rec loop ?(cmavg = 0.) ?(n = 0.) () =
    traceln "%s taking" prefix;
    let now = Eio.Time.now clock in
    Lf_token_bucket.take ~sw bucket;
    let dt = Eio.Time.now clock -. now in
    let n = n +. 1. in
    let cmavg = cmavg +. ((dt -. cmavg) /. n) in
    traceln "%s took %fs (avg: %fs)" prefix dt cmavg;
    loop ~cmavg ~n ()
  in
  loop ()

let consumers ~clock ?(prefix = "") ?(n = 10) bucket =
  let prefix n = Format.sprintf "%s [%d]:" prefix n in
  Switch.run @@ fun sw ->
  let spawn n () = consumer ~sw ~clock ~prefix:(prefix n) bucket in
  for i = 1 to n do
    Fiber.fork ~sw (spawn i);
    Fiber.yield ()
  done

let main_sc ?n env =
  let clock = Eio.Stdenv.clock env in
  let bucket = Lf_token_bucket.make ~capacity:10 1. in
  consumers ~clock ?n bucket

let main_mc env =
  let clock = Eio.Stdenv.clock env in
  let dmgr = Eio.Stdenv.domain_mgr env in
  Switch.run @@ fun sw ->
  let bucket = Lf_token_bucket.make ~capacity:10 ~init:0 1. in
  let spawn_worker name =
    Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run dmgr (fun () ->
            consumers ~clock ~prefix:name ~n:6 bucket))
  in
  for i = 1 to 6 do
    let asci = char_of_int (64 + i) in
    spawn_worker (Format.sprintf "[%c]" asci)
  done

let () =
  (* Eio_unix.Ctf.with_tracing "trace.ctf" @@ fun () -> *)
  (* Eio_luv.run @@ fun env -> main_sc env *)
  Eio_luv.run @@ fun env -> main_mc env
