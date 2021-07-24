open Containers
open Lwt.Infix
module Mpmc = Disco_utils.Mpmc

let rec iter_sink n s =
  Mpmc.Sink.pull s >>= function
  | Some v ->
      Printf.printf "(sink %d) %d\n%!" n v;
      iter_sink n s
  | None ->
      Printf.printf "(sink %d) dropped\n%!" n;
      Lwt.return_unit

let rec write_all src l =
  match l with
  | [] -> Lwt.return_unit
  | x :: xs -> Mpmc.Source.write src x >>= fun () -> write_all src xs

let base () =
  let open Lwt.Syntax in
  let source, sink = Mpmc.make () in
  let* sink2 =
    Mpmc.Sink.clone sink >>= fun s ->
    Mpmc.Sink.clone s >|= fun s2 -> s2
  in
  Gc.full_major ();

  let reads = Lwt.join [ iter_sink 2 sink2 ] in
  let writes =
    Lwt.join [ write_all source List.(1 -- 10); write_all source List.(1 -- 5) ]
    >>= fun () ->
    Mpmc.Source.close source >>= fun () -> iter_sink 1 sink
  in
  Lwt.join [ reads; writes ]

let gc () =
  let open Lwt.Syntax in
  let src, snk = Mpmc.make () in
  let clone_drop snk = Mpmc.Sink.clone snk >|= ignore in
  let* () = clone_drop snk in
  let* () = clone_drop snk in
  Gc.full_major ();
  Lwt.join
    [
      (write_all src List.(1 -- 5) >>= fun () -> Mpmc.Source.close src);
      iter_sink 1 snk;
    ]

let () =
  let runs = [ ("base", base); ("gc", gc) ] in
  let rec run = function
    | [] -> Lwt.return_unit
    | (name, fn) :: xs ->
        Printf.printf "\nrunning %s\n----------\n\n%!" name;
        fn () >>= fun () -> run xs
  in
  Lwt_main.run @@ run runs
