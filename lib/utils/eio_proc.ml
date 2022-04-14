open Eio.Std

type sink = < Eio.Generic.t ; Eio.Flow.sink ; Eio.Flow.close >
type source = < Eio.Generic.t ; Eio.Flow.source ; Eio.Flow.close >

let wrap_in src =
  let b = Bytes.create (1024 * 4) in
  let blen = Bytes.length b in
  object (_ : source)
    method probe _ = None

    method read_into buf =
      let rlen = min blen (Cstruct.length buf) in
      Eio_unix.await_readable src;
      match Unix.read src b 0 rlen with
      | 0 -> raise End_of_file
      | n ->
          Cstruct.blit_from_bytes b 0 buf 0 n;
          n

    method read_methods = []
    method close = Unix.close src
  end

let wrap_out snk =
  object (_ : sink)
    method probe _ = None

    method copy src =
      let buf = Cstruct.create_unsafe (1024 * 4) in
      let rec loop () =
        match Eio.Flow.read src buf with
        | (exception End_of_file) | 0 -> ()
        | n ->
            Eio_unix.await_writable snk;
            ignore @@ Unix.write snk (Cstruct.to_bytes buf) 0 n;
            loop ()
      in
      loop ()

    method close = Unix.close snk
  end

type proc = {
  pid : int;
  stdout : source;
  stdin : sink;
  stderr : source;
  mutable status : Unix.process_status option;
}

let pipe ~sw () =
  let r, w = Unix.pipe ~cloexec:true () in
  Switch.on_release sw (fun () ->
      try [ r; w ] |> List.iter Unix.close
      with Unix.Unix_error (EBADF, _, _) -> ());
  (r, w)

let rec waitpid_non_intr pid =
  try snd @@ Unix.waitpid [] pid
  with Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let stdout { stdout; _ } = stdout
let stdin { stdin; _ } = stdin
let stderr { stderr; _ } = stderr
let signal t signal = Unix.kill t.pid signal

let close_pipes t =
  try
    Eio.Flow.close t.stdout;
    Eio.Flow.close t.stdin;
    Eio.Flow.close t.stderr
  with Unix.Unix_error (EBADF, _, _) -> ()

let wait_set_status t =
  let s = waitpid_non_intr t.pid in
  t.status <- Some s;
  s

let kill t =
  match t.status with
  | None ->
      signal t Sys.sigkill;
      close_pipes t;
      wait_set_status t
  | Some s -> s

let close t =
  match t.status with
  | None ->
      close_pipes t;
      wait_set_status t
  | Some s -> s

let spawn ~sw prog =
  let stdout_r, stdout_w = pipe ~sw () in
  let stdin_r, stdin_w = pipe ~sw () in
  let stderr_r, stderr_w = pipe ~sw () in
  let pid = Unix.create_process_env prog [||] [||] stdin_r stdout_w stderr_w in
  [ stdout_w; stdin_r; stderr_w ] |> List.iter Unix.close;
  let stdout = wrap_in stdout_r in
  let stdin = wrap_out stdin_w in
  let stderr = wrap_in stderr_r in
  let t = { pid; stdout; stdin; stderr; status = None } in
  Switch.on_release sw (fun () -> ignore @@ kill t);
  t
