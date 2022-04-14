(* open! Disco_core.Globals
   module L = (val Relog.logger ~namespace:__MODULE__ ())

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
       "-nostdin";
       "-analyzeduration";
       "0";
       "-f";
       "s16le";
       "-ar";
       "48000";
       "-ac";
       "2";
       "-flush_packets";
       "1";
     ]

   let stdout_args = [ "-" ]

   let frame_reader () =
     let buf = Bigstringaf.create (Rtp._FRAME_SIZE * Rtp._CHANNELS * 2) in
     let blen = Bigstringaf.length buf in
     let parse_frame =
       let parser =
         Parser.s16le ~channels:Rtp._CHANNELS ~frame_size:Rtp._FRAME_SIZE
       in
       fun () -> parser buf
     in
     fun ic ->
       Lwt_io.read_into_exactly_bigstring ic buf 0 blen |> Lwt_result.catch
       >|= function
       | Ok () -> `Frame (parse_frame ())
       | Error End_of_file -> `Eof
       | Error exn -> `Exn exn

   let writer = function
     | `Stream s ->
         let f oc =
           Lwt_pipe.read s >>= function
           | Some d -> (
               Lwt_io.write_from_exactly_bigstring oc d 0 (Bigstringaf.length d)
               |> Lwt_result.catch
               >|= function
               | Ok () -> true
               | _ -> false)
           | None -> Lwt_io.close oc >|= Fun.const false
         in
         f
     | _ -> fun oc -> Lwt_io.close oc >|= Fun.const false

   let close_input = function `Stream s -> Lwt_pipe.close_nonblock s | _ -> ()

   let create input =
     let cmd = "ffmpeg" in
     let i =
       match input with
       | `Stream _ -> [ "-i"; "-" ]
       | `File path -> [ "-i"; path ]
       | `Url url -> [ "-i"; url ]
     in
     let args = List.concat [ i; pcm_args; stdout_args ] in
     Switch.run @@ fun sw ->
     let cmd_str = cmd :: args |> List.to_string ~sep:" " Fun.id in
     L.debug (fun m -> m "running cmd: %s" cmd_str);
     let init () = Eio_proc.spawn ~sw in
     let stop proc = Eio_proc.close proc |> ignore in
     let pull proc = None in

     let p = Lwt_pipe.create () in
     let p_p, u_p = Lwt.wait () in
     let spawn () =
       let cmd_str = cmd :: args |> List.to_string ~sep:" " Fun.id in
       L.debug (fun m -> m "running cmd: %s" cmd_str);

       Lwt_process.with_process_full
         ("", Array.of_list @@ (cmd :: args))
         (fun proc ->
           L.debug (fun m -> m "ffmpeg running with pid=%d" proc#pid);

           let read_logs () =
             Lwt_io.read ~count:0x100 proc#stderr >|= fun s ->
             `Logs (String.length s = 0)
           in
           let read_frame () = frame_reader () proc#stdout in
           let write () = writer input proc#stdin >|= fun r -> `Write r in

           let rec poll' q =
             Lwt.nchoose_split q >>= function
             | [], [] -> Lwt_result.return ()
             | rs, ps -> (
                 match handle ~out:ps rs with
                 | Ok [] -> Lwt_result.return ()
                 | Ok q -> poll' q
                 | Error _ as e -> Lwt.return e)
           and handle ?(out = []) = function
             | [] -> Ok out
             | (`Write false | `Eof | `Logs true) :: xs -> handle ~out xs
             | `Logs false :: xs -> handle ~out:(read_logs () :: out) xs
             | `Frame frame :: xs ->
                 if Lwt.is_sleeping p_p then Lwt.wakeup_later u_p (Ok p);
                 let fwd = Lwt_pipe.write p frame >|= fun r -> `Fwd r in
                 handle ~out:(fwd :: out) xs
             | `Fwd true :: xs ->
                 let r = read_frame () in
                 handle ~out:(r :: out) xs
             | `Fwd false :: _ ->
                 L.warn (fun m -> m "receiving end of pipe closed, cleaning up...");
                 List.iter Lwt.cancel out;
                 let clean =
                   Lwt_io.abort proc#stdin >>= fun () ->
                   Lwt_io.abort proc#stdout >>= fun () ->
                   Lwt_io.abort proc#stderr >|= fun () ->
                   proc#terminate;
                   `Eof
                 in
                 Ok [ clean ]
             | `Write true :: xs ->
                 let w = write () in
                 handle ~out:(w :: out) xs
             | (`Exn _ as e) :: _ ->
                 List.iter Lwt.cancel out;
                 L.err (fun m -> m "boop %a" Error.pp e);
                 Error e
           in

           let res =
             let open Lwt_result.Syntax in
             let* () =
               poll' [ read_frame (); write () ] >>= function
               | Error _ as e ->
                   Lwt_pipe.close_nonblock p;
                   close_input input;
                   proc#close >|= Fun.const e
               | Ok () as o -> Lwt.return o
             in
             L.info (fun m -> m "closing the process");
             proc#close >|= function
             | Unix.WEXITED 0 when Lwt.is_sleeping p_p ->
                 Error.msg "0 byte stream?"
             | Unix.WEXITED 0 -> Ok ()
             | Unix.WEXITED n -> Error.msgf "non 0 status code: %d" n
             | Unix.WSIGNALED n | Unix.WSTOPPED n ->
                 L.warn (fun m -> m "stopped with code: %d" n);
                 Ok ()
           in

           res >|= fun r ->
           L.err (fun m -> m "oyy m8");
           match (Lwt.is_sleeping p_p, r) with
           | true, (Error _ as e) -> Lwt.wakeup_later u_p e
           | true, Ok () -> failwith "unreachable"
           | false, Error e -> L.err (fun m -> m "%s" (Error.to_string e))
           | false, Ok () -> ())
     in
     let k =
       Lwt.catch spawn (fun e ->
           L.err (fun m -> m "spawn error: %a" Error.pp (`Exn e));
           Lwt.return_unit)
     in
     Lwt_pipe.keep p k;
     Lwt.on_termination k (fun () ->
         L.err (fun m -> m "HALP");
         Lwt_pipe.close_nonblock p);
     p_p |> Lwt_result.map of_pipe *)
