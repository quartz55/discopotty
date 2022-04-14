(* module D = Disco
   open Containers
   open Lwt.Infix

   module L = (val Relog.logger ~namespace:"Ytdl" ())

   module File_stream = struct
     module L = (val Relog.clone (module L) ~namespace:"File_stream")

     type op = Live of int | Draining of int | Scheduled of unit Lwt.u

     type reader = { ic : Lwt_io.input_channel; mutable op : op }

     type t = {
       path : string;
       oc : Lwt_io.output_channel;
       mutable readers : reader Ke.Fke.t;
       mutable written : int;
     }

     let create path =
       let oflags = Unix.[ O_WRONLY; O_CREAT ] in
       let readers = Ke.Fke.empty in
       Lwt_io.open_file ~mode:Lwt_io.Output ~flags:oflags path >|= fun oc ->
       { path; oc; readers; written = 0 }

     let notify_readers t written =
       let rec notify ?(acc = Ke.Fke.empty) k =
         match Ke.Fke.pop k with
         | Some ({ ic; _ }, xs) when Lwt_io.is_closed ic -> notify ~acc xs
         | Some (({ op = Live n; _ } as r), xs) ->
             r.op <- (if written = 0 then Draining n else Live (n + written));
             notify ~acc:(Ke.Fke.push acc r) xs
         | Some (({ op = Scheduled u; _ } as r), xs) ->
             r.op <- (if written = 0 then Draining 0 else Live written);
             Lwt.wakeup_later u ();
             notify ~acc:(Ke.Fke.push acc r) xs
         | Some (({ op = Draining _; _ } as r), xs) ->
             if written <> 0 then failwith "write after close?";
             notify ~acc:(Ke.Fke.push acc r) xs
         | None -> acc
       in
       t.readers <- notify t.readers

     let write t ?(off = 0) ?len b =
       if Lwt_io.is_closed t.oc then
         failwith "File_stream: cannot write to closed file";
       let len = Option.get_lazy (fun () -> Bigstringaf.length b - off) len in
       Lwt_io.write_from_exactly_bigstring t.oc b off len >|= fun () ->
       t.written <- t.written + len;
       notify_readers t len

     let reader t =
       let iflags = Unix.[ O_RDONLY ] in
       Lwt_io.open_file ~mode:Lwt_io.Input ~flags:iflags t.path >|= fun ic ->
       let op =
         if Lwt_io.is_closed t.oc then Draining t.written else Live t.written
       in
       let r = { ic; op } in
       t.readers <- Ke.Fke.push t.readers r;
       r

     let close t = Lwt_io.close t.oc >|= fun () -> notify_readers t 0

     module Reader = struct
       let rec read t ?(off = 0) ?len b =
         let len = Option.get_lazy (fun () -> Bigstringaf.length b - off) len in
         match t.op with
         | _ when Lwt_io.is_closed t.ic -> Lwt.return None
         | Scheduled _ -> failwith "File_stream: read already scheduled"
         | Draining 0 ->
             (if not @@ Lwt_io.is_closed t.ic then Lwt_io.close t.ic
             else Lwt.return_unit)
             >|= fun () -> None
         | Live 0 ->
             let p, u = Lwt.wait () in
             t.op <- Scheduled u;
             p >>= fun () -> read t ~off ~len b
         | (Live avail | Draining avail) as op ->
             Lwt_io.read_into_bigstring t.ic b off (min len avail) >>= fun n ->
             t.op <-
               (match op with
               | Live _ -> Live (avail - n)
               | Draining _ -> Draining (avail - n)
               | _ -> failwith "unreachable");
             Lwt.return @@ Some n

       let close t = Lwt_io.close t.ic

       let to_pipe t =
         let buf = Bigstringaf.create 0x1000 in
         let p = Lwt_pipe.create () in
         let rec fwd () =
           read t buf >>= function
           | None -> Lwt.return_unit
           | Some n ->
               Lwt_pipe.write p (Bigstringaf.copy buf ~off:0 ~len:n) >>= fun ok ->
               if ok then fwd () else close t
         in
         let f = Lwt.finalize fwd (fun () -> close t) in
         Lwt.on_termination f (fun () -> Lwt_pipe.close_nonblock p);
         Lwt_pipe.keep p f;
         p
     end
   end

   type track = {
     id : string;
     title : string;
     url : string;
     thumbnail : string option; [@yojson.option]
   }
   [@@deriving show, yojson] [@@yojson.allow_extra_fields]

   let base_args =
     [ "--default-search"; "ytsearch1:"; "-f"; "webm[abr>0]/bestaudio/best" ]

   module Store = struct
     type t = { dir : string; max_size : int }

     let kb = 1024

     let mb = 1024 * kb

     let create ?(max_size = 500 * mb) ?(dir = "./.discostore") () =
       let open Result.Infix in
       let+ () =
         match Sys.is_directory dir with
         | false -> Error.msgf "'%s' already exists and is not a directory" dir
         | true -> Ok ()
         | exception Sys_error _ ->
             Sys.mkdir dir 0o751;
             Ok ()
       in
       { dir; max_size }

     let path t filename = Filename.concat t.dir filename [@@inline]

     let get t filename =
       let path = path t filename in
       if Sys.file_exists path then Some path else None
   end

   type t = { store : Store.t; bin : string }

   let query t q =
     let cmd = t.bin in
     let playlist = String.find ~sub:"list=" q <> -1 in
     let args = base_args @ [ "-j"; q ] in
     let args = if playlist then "--flat-playlist" :: args else args in
     let cmd_str = cmd :: args |> List.to_string ~sep:" " Fun.id in
     L.info (fun m -> m "running cmd: %s" cmd_str);
     Lwt_process.with_process_full
       ("", Array.of_list @@ cmd :: args)
       (fun proc ->
         let _logs () = Lwt_io.read proc#stderr in
         L.info (fun m -> m "youtube-dl running with pid=%d" proc#pid);
         (Lwt_io.read_line proc#stdout |> Lwt_result.catch >|= function
          | Error End_of_file -> Ok []
          | Error exn -> Error.exn exn
          | Ok track ->
              let track_json = Yojson.Safe.from_string track in
              let track = track_of_yojson track_json in
              Ok [ track ])
         >>= fun res ->
         proc#close >|= fun _ -> res)

   let download t track =
     let open Lwt.Syntax in
     let filename = Store.path t.store track.id in
     let p, u = Lwt.wait () in
     let w () =
       Lwt_process.with_process_full
         ("", [| t.bin; "-a"; "-"; "-o"; "-" |])
         (fun proc ->
           let* () = Lwt_io.write proc#stdin track.url in
           let* () = Lwt_io.close proc#stdin in
           let* sf = File_stream.create filename in
           let buf = Bigstringaf.create 0x1000 in
           let rec poll () =
             Lwt_io.read_into_bigstring proc#stdout buf 0 0x1000 >>= function
             | 0 -> File_stream.close sf
             | n ->
                 let* () =
                   if Lwt.is_sleeping p then
                     File_stream.reader sf >|= File_stream.Reader.to_pipe
                     >|= fun s -> Lwt.wakeup_later u (Ok s)
                   else Lwt.return_unit
                 in
                 File_stream.write sf ~off:0 ~len:n buf >>= poll
           in
           poll () >>= fun () ->
           proc#close >>= function
           | Unix.WEXITED 0 ->
               if Lwt.is_sleeping p then Lwt.return (Error.msg "0 byte stream?")
               else Lwt_result.return ()
           | WEXITED n | WSTOPPED n | WSIGNALED n ->
               Lwt.return (Error.msgf "youtube-dl returned non 0 code: %d" n))
     in
     Lwt.async (fun () ->
         w () >|= fun r ->
         match (Lwt.is_sleeping p, r) with
         | true, (Error _ as e) -> Lwt.wakeup_later u e
         | true, Ok () -> ()
         | false, _ -> ());
     p

   let stream path =
     let open Lwt_result.Syntax in
     let+ ic = Lwt_io.open_file ~mode:Lwt_io.input path |> Error.catch_lwt in
     let p = Lwt_pipe.create () in
     let b = Bigstringaf.create 0x1000 in
     let blen = Bigstringaf.length b in
     let rec fwd () =
       Lwt_io.read_into_bigstring ic b 0 blen >>= function
       | 0 -> Lwt.return_unit
       | len ->
           Lwt_pipe.write p (Bigstringaf.copy b ~off:0 ~len) >>= fun ok ->
           if ok then fwd () else Lwt.return_unit
     in
     let f = Lwt.finalize fwd (fun () -> Lwt_io.close ic) in
     Lwt.on_termination f (fun () -> Lwt_pipe.close_nonblock p);
     Lwt_pipe.keep p f;
     p

   let get t q =
     query t q >>= function
     | (Ok [] | Error _) as o -> Lwt.return o
     | Ok (track :: _) -> (
         let f =
           Store.get t.store track.id
           |> Option.map (fun path ->
                  L.debug (fun m -> m "track file '%s' cached in store" path);
                  stream path |> Lwt_result.map (fun s -> `Stream s))
           |> Option.get_lazy (fun () ->
                  L.debug (fun m ->
                      m "track '%s' not cached, downloading..." track.id);
                  download t track |> Lwt_result.map (fun s -> `Stream s))
         in
         f >|= function Ok i -> Ok [ (track, i) ] | Error _ as e -> e)

   let create ?(bin = "youtube-dl") ?store () =
     let open Result.Infix in
     let* () =
       match Sys.command (bin ^ " --version") with
       | 0 -> Ok ()
       | _ -> Error.msgf "'%s' is not a valid youtube-dl binary" bin
     in
     let+ store = Option.map Result.return store |> Option.get_lazy Store.create in
     { bin; store } *)
