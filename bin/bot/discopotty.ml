open Containers
module D = Discord
module M = Discord_models
module Msg = M.Message

module L = (val Relog.logger ~namespace:__MODULE__ ())

let setup_logging () =
  let open Relog in
  let v =
    Option.(
      Sys.getenv_opt "RELOG_LEVEL" >>= fun lvl ->
      choice
        [
          Int.of_string lvl >>= Level.of_verbosity;
          Level.of_string (String.lowercase_ascii lvl);
        ])
    |> Option.get_or ~default:Level.Debug
  in
  let cli_fmter = Formatter.default ~color:true ~oneline:false () in
  let cli_fmt = Format.formatter_of_out_channel stderr in
  Sink.make (fun r ->
      if Level.Infix.(Record.level r <= v) then cli_fmter cli_fmt r else ())
  |> Relog.Sink.set

let handler cfg client =
  let prefix = Config.prefix cfg in
  Lwt_unix.on_signal Sys.sigint (fun _ ->
      Lwt.dont_wait (fun () -> D.Client.disconnect client) ignore)
  |> ignore;
  let open Lwt.Syntax in
  function
  | D.Events.Message_create { content; channel_id; guild_id; _ } -> (
      L.warn (fun m -> m "MESSAGE: %s" content);
      match Cmd.of_message ~prefix content with
      | None -> Lwt.return ()
      | Some ("ping", args) ->
          let msg = Msg.fmt "@{<b>pong@} %s" args in
          D.Client.send_message channel_id msg client
      | Some ("suicide", _args) ->
          let msg =
            Msg.fmt "⚠️ @{<b>@{<i>disconnecting by user request...@}@} 👋"
          in
          let* () = D.Client.send_message channel_id msg client in
          D.Client.disconnect client
      | Some ("join", "") ->
          let msg =
            Msg.fmt
              "⚠️ Not supported yet, please provide a voice channel id"
          in
          D.Client.send_message channel_id msg client
      | Some ("join", v_channel_id) ->
          let guild_id = Option.get_exn guild_id in
          let v_channel_id = M.Snowflake.of_string v_channel_id in
          D.Client.join_voice ~guild_id ~channel_id:v_channel_id client
      | Some ("play", "kiff") -> (
          let guild_id = Option.get_exn guild_id in
          let* s = D.Audio_stream.Ffmpeg.create "./kiff.mp3" in
          match s with
          | Ok audio_stream ->
              D.Client.play_audio_stream ~guild_id audio_stream client
          | _ -> Lwt.return_unit)
      | Some ("play", "soundbite") -> (
          let guild_id = Option.get_exn guild_id in
          let* s = D.Audio_stream.Ffmpeg.create "./bite2.mp3" in
          match s with
          | Ok audio_stream ->
              D.Client.play_audio_stream ~guild_id audio_stream client
          | _ -> Lwt.return_unit)
      | Some ("play", "allstar") -> (
          let guild_id = Option.get_exn guild_id in
          let* s = D.Audio_stream.Ffmpeg.create "./allstar.mp3" in
          match s with
          | Ok audio_stream ->
              D.Client.play_audio_stream ~guild_id audio_stream client
          | _ -> Lwt.return_unit)
      | Some ("play", query) -> (
          let guild_id = Option.get_exn guild_id in
          let* tracks = Ytdl.query query in
          match tracks with
          | Ok [] | Error _ ->
              let msg = Msg.fmt "⚠️ No track found for query: '%s'" query in
              D.Client.send_message channel_id msg client
          | Ok (track :: _) -> (
              let* s = D.Audio_stream.Ffmpeg.create track.url in
              match s with
              | Ok audio_stream ->
                  let* () =
                    D.Client.play_audio_stream ~guild_id audio_stream client
                  in
                  let msg =
                    Msg.fmt "🎵 @{<b>Now playing:@} '%s'\n%s" track.title
                      track.thumbnail
                  in
                  D.Client.send_message channel_id msg client
              | Error e ->
                  client
                  |> D.Client.send_message channel_id
                       (Msg.fmt "⚠️ Couldn't play track: '%s'\nReason: %a"
                          track.title D.Error.pp e)))
      | Some ("pause", _) ->
          let _guild_id = Option.get_exn guild_id in
          Lwt.return_unit
      | Some (other, _) ->
          let msg =
            Msg.fmt "🛑 @{<b>unsupported command@} @{<code>%s@}" other
          in
          D.Client.send_message channel_id msg client)
  | _ ->
      L.debug (fun m -> m "don't care");
      Lwt.return ()

let () =
  Gc.(set { (get ()) with minor_heap_size = 256000 * 4 });
  let inner () =
    let open Result.Infix in
    setup_logging ();
    let* config = Config.of_filename "./discopotty.toml" in
    let token = Config.token config in
    Lwt_main.run (D.Client.create ~handler:(handler config) token)
    |> Result.map_err D.Error.to_string
  in
  match inner () with
  | Ok () -> ()
  | Error error -> prerr_endline ("Error: " ^ error)
