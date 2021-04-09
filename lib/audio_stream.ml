open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

open Lwt.Infix

let silence_frame =
  [ '\xf8'; '\xff'; '\xfe' ] |> String.of_list
  |> Bigstringaf.of_string ~off:0 ~len:3

let silence_stream = Lwt_stream.from_direct (fun () -> Some silence_frame)

let n_silence_pipe ?(n = 10) () =
  Lwt_pipe.of_list (List.init n (fun _ -> silence_frame))

let of_pipe p =
  let out = Lwt_pipe.create () in
  let rec poll' () =
    Lwt_pipe.read p >>= function
    | None -> Lwt.return_unit
    | Some data -> (
        Lwt_pipe.write out data >>= function
        | true -> poll' ()
        | false -> Lwt.return_unit)
  in
  let fwd = poll' () in
  Lwt_pipe.keep out fwd;
  Lwt.on_termination fwd (fun () -> Lwt_pipe.close_nonblock out);
  out

module Gen = struct
  (** Fractional part of a float. *)
  let fracf x = if x <. 1. then x else if x <. 2. then x -. 1. else fst (modf x)

  let s16 =
    let r = Int16.(max_int |> to_float) in
    fun f -> Int.of_float (Float.(min 1. (max (-1.) f)) *. r)

  let sine ?(freq = 440) ?(phase = 0.) ?(samplerate = 48000) ?(framelen = 20)
      ?(channels = 2) duration =
    let phase = ref phase in
    let framesize = samplerate / 1000 * framelen in
    let volume = 0.5 in
    let omega = float freq /. float samplerate in
    let gen =
      let buf =
        Bigarray.(Array1.create float32 c_layout (framesize * channels))
      in
      fun () ->
        for i = 0 to framesize - 1 do
          let sample = volume *. sin ((float i *. omega) +. !phase) in
          (* let sample = s16 sample in *)
          for c = 0 to channels - 1 do
            buf.{i + c} <- sample
          done
        done;
        phase :=
          mod_float (!phase +. (float framesize *. omega)) (2. *. Float.pi);
        buf
    in
    let p = Lwt_pipe.create () in
    let rec write = function
      | 0 -> Lwt.return_unit
      | n ->
          Lwt_pipe.write p (gen ()) >>= fun ok ->
          if ok then write (n - 1) else Lwt.return_unit
    in
    let nframes = Int.of_float (duration *. 1e3) / framelen in
    let fut = write nframes in
    Lwt_pipe.keep p fut;
    Lwt.on_termination fut (fun () -> Lwt_pipe.close_nonblock p);
    p
end

module Ffmpeg = struct
  module L = (val Relog.clone (module L) ~namespace:"Ffmpeg")

  module Parser = struct
    let frame ~sample_p ~channels ~size ~kind =
      let buf =
        Bigarray.Array1.create kind Bigarray.c_layout (channels * size)
      in
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
    [ "-analyzeduration"; "0"; "-f"; "s16le"; "-ar"; "48000"; "-ac"; "2" ]

  (* let ogg_args =
     [ "-analyzeduration"; "0"; "-f"; "s16le"; "-ar"; "48000"; "-ac"; "2" ] *)

  let stdout_args = [ "-" ]

  let of_file filename =
    let cmd = "ffmpeg" in
    let args = List.concat [ [ "-i"; filename ]; pcm_args; stdout_args ] in

    let p = Lwt_pipe.create () in
    let p_p, u_p = Lwt.wait () in
    let spawn () =
      let cmd_str = cmd :: args |> List.to_string ~sep:" " Fun.id in
      L.info (fun m -> m "running cmd: %s" cmd_str);
      Lwt_process.with_process_full
        ("", Array.of_list @@ cmd :: args)
        (fun proc ->
          let raw = proc#stdout in
          let logs () = Lwt_io.read proc#stderr in
          L.info (fun m -> m "ffmpeg running with pid=%d" proc#pid);
          let buf = Bigstringaf.create (Rtp._FRAME_SIZE * Rtp._CHANNELS * 2) in
          let buf_len = Bigstringaf.length buf in
          let parse_frame =
            let parser =
              Parser.s16le ~channels:Rtp._CHANNELS ~frame_size:Rtp._FRAME_SIZE
            in
            fun () -> parser buf
          in
          let rec poll' () =
            L.trace (fun m -> m "polling");
            Lwt_io.read_into_exactly_bigstring raw buf 0 buf_len
            |> Lwt_result.catch
            >>= function
            | Ok () ->
                L.trace (fun m -> m "parsing frame");
                let frame = parse_frame () in
                if Lwt.is_sleeping p_p then Lwt.wakeup_later u_p (Ok p);
                Lwt_pipe.write p frame >>= fun ok ->
                if ok then poll' () else proc#close >|= ignore
            | Error End_of_file -> Lwt.return_unit
            | Error exn -> Lwt.fail exn
          in
          let open Lwt.Syntax in
          let* status = poll' () >>= fun () -> proc#status in
          let+ res =
            match status with
            | Unix.WEXITED 0 ->
                if Lwt.is_sleeping p_p then
                  Lwt_result.fail (`Msg "0 byte stream?")
                else Lwt_result.return ()
            | WEXITED n ->
                L.error (fun m ->
                    m "got non 0 status code for ffmpeg (status=%d)" n);
                let+ logs = logs () in
                L.error (fun m -> m "logs:@.%s" logs);
                Error
                  (`Msg
                    (Printf.sprintf
                       "got non 0 status code for ffmpeg (status=%d)\n%s" n logs))
            | WSIGNALED n | WSTOPPED n ->
                L.warn (fun m -> m "ffmpeg process was closed with code=%d" n);
                Lwt_result.return ()
          in
          match (Lwt.is_sleeping p_p, res) with
          | true, (Error _ as e) -> Lwt.wakeup_later u_p e
          | true, Ok () -> ()
          | false, _ -> ())
    in
    let k = spawn () in
    Lwt_pipe.keep p k;
    Lwt.on_termination k (fun () -> Lwt_pipe.close_nonblock p);
    p_p |> Lwt_result.map of_pipe
end
