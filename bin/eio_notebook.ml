module Eio_proc = Disco_utils.Eio_proc

let read_str flow =
  let out = Buffer.create 1024 in
  let buf = Cstruct.create_unsafe 1024 in
  let rec read () =
    match Eio.Flow.read flow buf with
    | (exception End_of_file) | 0 ->
        Buffer.to_bytes out |> Bytes.unsafe_to_string
    | n ->
        Buffer.add_bytes out (Cstruct.to_bytes ~len:n buf);
        read ()
  in
  read ()

let worker n =
  Eio.Switch.run @@ fun sw ->
  let str = Format.sprintf "worker %d" n in
  let proc = Eio_proc.spawn ~sw "cat" in
  Eio.Flow.copy_string str (Eio_proc.stdin proc);
  Eio.Flow.close (Eio_proc.stdin proc);
  let logs = read_str (Eio_proc.stderr proc) in
  Eio.traceln "logs: %s" logs;
  let str_out = read_str (Eio_proc.stdout proc) in
  Eio.traceln "%s vs %s" str str_out;
  assert (String.equal str str_out)

let () =
  Eio_luv.run @@ fun env ->
  let _dmgr = Eio.Stdenv.domain_mgr env in
  Eio.Switch.run @@ fun _sw -> worker 0
(* for i = 1 to 10 do
     Eio.Fiber.fork ~sw @@ fun () ->
     Eio.Domain_manager.run dmgr @@ fun () -> worker i
   done *)
