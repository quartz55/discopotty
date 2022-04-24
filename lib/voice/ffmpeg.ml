open! Disco_core.Globals
open Eio.Std
module L = (val Relog.logger ~namespace:__MODULE__ ())

exception Invalid_input of string

module Parser = struct
  let frame ~sample_p ~channels ~size ~kind =
    let buf = Bigarray.Array1.create kind Bigarray.c_layout (channels * size) in
    let p =
      let open Angstrom in
      let sample = count channels sample_p in
      let rec samples ?(i = 0) () =
        end_of_input >>| (fun () -> `eof) <|> (sample >>| fun s -> `sample s)
        >>= function
        | `eof -> return ()
        | `sample chans ->
            List.iteri (fun ci c -> buf.{i + ci} <- c) chans;
            samples ~i:(i + channels) ()
      in
      samples () >>| fun () -> buf
    in
    fun buf ->
      Angstrom.parse_bigstring ~consume:Angstrom.Consume.All p buf |> function
      | Ok frame -> frame
      | Error str -> failwith @@ "error parsing raw audio frame " ^ str

  let s16le ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.LE.any_int16 ~channels ~size:frame_size
      ~kind:Bigarray.int16_signed

  let s16be ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.BE.any_int16 ~channels ~size:frame_size
      ~kind:Bigarray.int16_signed

  let f32le ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.LE.any_float ~channels ~size:frame_size
      ~kind:Bigarray.float32

  let f32be ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.BE.any_float ~channels ~size:frame_size
      ~kind:Bigarray.float32

  let f64le ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.LE.any_double ~channels ~size:frame_size
      ~kind:Bigarray.float64

  let f64be ?(channels = 2) ?(frame_size = 48000) =
    frame ~sample_p:Angstrom.BE.any_double ~channels ~size:frame_size
      ~kind:Bigarray.float64
end

let pcm_args =
  [
    (* "-nostdin"; *)
    "-f";
    "s16le";
    "-ar";
    "48000";
    "-ac";
    "2";
    "-flush_packets";
    "1";
  ]

let stdout_args = [ "pipe:" ]

let frame_reader () =
  let buf = Bigstringaf.create (Rtp._FRAME_SIZE * Rtp._CHANNELS * 2) in
  let c_buf = Cstruct.of_bigarray buf in
  let parse_frame =
    let parser =
      Parser.s16le ~channels:Rtp._CHANNELS ~frame_size:Rtp._FRAME_SIZE
    in
    fun () -> parser buf
  in
  fun flow ->
    match Eio.Flow.read_exact flow c_buf with
    | () -> Some (parse_frame ())
    | exception End_of_file -> None

let process ~sw input =
  (* TODO @quartz55:  better handle this for use cases like nix *)
  let cmd = "ffmpeg" in
  let i =
    match input with
    | `Stream _ -> [ "-analyzeduration"; "0"; "-i"; "-" ]
    | `File path -> [ "-analyzeduration"; "-i"; path ]
    | `Url url -> [ "-i"; url ]
  in
  let args = List.concat [ i; pcm_args; stdout_args ] in
  let p, u = Promise.create () in
  let spawn () =
    Switch.run @@ fun sw ->
    let cmd_str = cmd :: args |> List.to_string ~sep:" " Fun.id in
    L.debug (fun m -> m "running cmd: %s" cmd_str);
    let proc = Eio_proc.spawn ~sw cmd ~args in
    L.debug (fun m -> m "ffmpeg running with pid=%d" (Eio_proc.pid proc));
    let stdin, stderr, stdout =
      Eio_proc.(stdin proc, stderr proc, stdout proc)
    in
    let read = frame_reader () in
    let logs_thread =
      Fiber.fork_promise ~sw @@ fun () ->
      let logs_b = Buffer.create (80 * 40) in
      let logs = Eio.Flow.buffer_sink logs_b in
      Eio.Flow.copy stderr logs;
      Buffer.to_bytes logs_b |> Bytes.unsafe_to_string
    in
    let cleanup () =
      let status = Eio_proc.close proc in
      match Promise.await_exn logs_thread with
      | logs -> (status, logs)
      | exception exc ->
          let reason = Printexc.to_string exc in
          L.warn (fun m -> m "couldn't get ffmpeg logs: %s" reason);
          (status, Format.sprintf "<no logs>: reason: %s" reason)
    in
    let write_thread () =
      match input with
      | `Stream flow -> Eio.Flow.copy flow stdin
      | _ -> Eio.Flow.close stdin
    in
    Fiber.fork ~sw write_thread;
    (* try to read at least one frame to check if valid input *)
    match read stdout with
    | Some f ->
        let done_p, done_u = Promise.create () in
        let init () = true in
        let pull first =
          (* TODO error handling *)
          if first then Some (f, false)
          else read stdout |> Option.map (fun f -> (f, false))
        in
        let stop _ =
          let _ = cleanup () in
          if not @@ Promise.is_resolved done_p then Promise.resolve done_u ()
        in
        let src = Streaming.Source.make ~init ~pull ~stop () in
        Promise.resolve_ok u src;
        Promise.await done_p;
        raise Exit
    | None ->
        let status, logs = cleanup () in
        let status_str =
          match status with
          | Unix.WEXITED n -> "exited " ^ string_of_int n
          | Unix.WSIGNALED n -> "signaled " ^ string_of_int n
          | Unix.WSTOPPED n -> "stopped " ^ string_of_int n
        in
        let exc =
          Invalid_input (Format.sprintf "status: %s logs: %s" status_str logs)
        in
        Promise.resolve_error u exc
    | exception exc ->
        let status, logs = cleanup () in
        let status_str =
          match status with
          | Unix.WEXITED n -> "exited " ^ string_of_int n
          | Unix.WSIGNALED n -> "signaled " ^ string_of_int n
          | Unix.WSTOPPED n -> "stopped " ^ string_of_int n
        in
        let exc =
          Invalid_input
            (Format.sprintf "exc: %s status: %s logs: %s"
               (Printexc.to_string exc) status_str logs)
        in
        Promise.resolve_error u exc
  in
  Fiber.fork ~sw (fun () -> try spawn () with Exit -> ());
  Promise.await_exn p |> Audio_stream.of_raw_s16
