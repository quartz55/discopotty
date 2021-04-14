open Containers
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

type track = { title : string; url : string; thumbnail : string }

let args =
  [ "--default-search"; "ytsearch1:"; "-f"; "webm[abr>0]/bestaudio/best"; "-j" ]

let query q =
  let cmd = "youtube-dl" in
  let playlist = String.find ~sub:"list=" q <> -1 in
  let args = args @ [ q ] in
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
       | Error exn -> Discord.Error.exn exn
       | Ok track ->
           let info = Yojson.Safe.from_string track in
           let title = Yojson.Safe.Util.(member "title" info |> to_string) in
           let url = Yojson.Safe.Util.(member "url" info |> to_string) in
           let thumbnail =
             Yojson.Safe.Util.(member "thumbnail" info |> to_string)
           in
           Ok [ { title; url; thumbnail } ])
      >>= fun res ->
      proc#close >|= fun _ -> res)
