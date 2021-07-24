open Containers
open Lwt.Infix
module D = Disco
module Voice = Disco_voice
module Client = D.Client.Make (Voice.Manager)
module M = Disco_models
module Msg = M.Message

module L = (val Relog.logger ~namespace:"Discopotty" ())

let setup_logging () =
  let open Relog in
  let v =
    Option.(
      Sys.getenv_opt "LOG_LEVEL" >>= fun lvl ->
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

let handler cfg _ytdl client =
  let prefix = Config.prefix cfg in
  Lwt_unix.on_signal Sys.sigint (fun _ ->
      Lwt.dont_wait (fun () -> Client.disconnect client) ignore)
  |> ignore;
  let open Lwt.Syntax in
  function
  | Disco_core.Events.Message_create { content; channel_id; guild_id; _ } -> (
      match Cmd.of_message ~prefix content with
      | None -> Lwt.return ()
      | Some ("ping", args) ->
          let msg = Msg.fmt "@{<b>pong@} %s" args in
          Client.send_message channel_id msg client
      | Some ("suicide", _args) ->
          let msg =
            Msg.fmt "⚠️ @{<b>@{<i>disconnecting by user request...@}@} 👋"
          in
          let* () = Client.send_message channel_id msg client in
          Client.disconnect client
      | Some ("status", st) ->
          let st =
            match st with
            | "offline" -> `offline
            | "invisible" -> `invisible
            | "idle" -> `idle
            | "dnd" -> `dnd
            | _ -> `online
          in
          let gw = Client.gateway client in
          Disco_core.Gateway.send_presence_update gw ~afk:false st
      | Some ("join", "") ->
          let msg =
            Msg.fmt
              "⚠️ Not supported yet, please provide a voice channel id"
          in
          Client.send_message channel_id msg client
      | Some ("join", vchan) -> (
          let guild_id = Option.get_exn guild_id in
          let vchan = M.Snowflake.of_string vchan in
          let voice = Client.voice client in
          let call = Voice.Manager.get ~guild_id voice in
          Voice.Manager.Call.join call ~channel_id:vchan >>= function
          | Error e ->
              let msg =
                Msg.fmt "⚠️ Couldn't join voice channel: %a" Voice.Error.pp
                  e
              in
              Client.send_message channel_id msg client
          | Ok () -> Lwt.return_unit)
      | Some ("leave", _) ->
          let guild_id = Option.get_exn guild_id in
          let voice = Client.voice client in
          let call = Voice.Manager.get ~guild_id voice in
          Voice.Manager.Call.leave call
      (* | Some ("join", v_channel_id) ->
             let guild_id = Option.get_exn guild_id in
             let channel_id = M.Snowflake.of_string v_channel_id in
             let voice = Client.voice client in
             Voice.Manager.join ~guild_id ~channel_id voice
         | Some ("play", "soundbite") -> (
             let guild_id = Option.get_exn guild_id in
             let* s = Voice.Audio_stream.Ffmpeg.create @@ `File "./bite2.mp3" in
             match s with
             | Ok audio_stream ->
                 Client.play_audio_stream ~guild_id audio_stream client
             | _ -> Lwt.return_unit)
         | Some ("play", "kiff") -> (
             let guild_id = Option.get_exn guild_id in
             let* s = Voice.Audio_stream.Ffmpeg.create @@ `File "./kiff.mp3" in
             match s with
             | Ok audio_stream ->
                 Client.play_audio_stream ~guild_id audio_stream client
             | _ -> Lwt.return_unit)
         | Some ("play", query) -> (
             let guild_id = Option.get_exn guild_id in
             let* tracks = Ytdl.get ytdl query in
             match tracks with
             | Ok [] | Error _ ->
                 let msg = Msg.fmt "⚠️ No track found for query: '%s'" query in
                 Client.send_message channel_id msg client
             | Ok ((track, i) :: _) -> (
                 let* s = Voice.Audio_stream.Ffmpeg.create i in
                 match s with
                 | Ok audio_stream ->
                     let* () =
                       Client.play_audio_stream ~guild_id audio_stream client
                     in
                     let thumb fmt = function
                       | Some t -> Format.fprintf fmt "\n%s" t
                       | None -> ()
                     in
                     let msg =
                       Msg.fmt "🎵 @{<b>Now playing:@} '%s'%a" track.title thumb
                         track.thumbnail
                     in
                     Client.send_message channel_id msg client
                 | Error e ->
                     client
                     |> Client.send_message channel_id
                          (Msg.fmt "⚠️ Couldn't play track: '%s'\nReason: %a"
                             track.title Error.pp e))) *)
      | Some (other, _) ->
          let msg =
            Msg.fmt "🛑 @{<b>unsupported command@} @{<code>%s@}" other
          in
          Client.send_message channel_id msg client)
  | _ ->
      L.debug (fun m -> m "don't care");
      Lwt.return ()

let () =
  Gc.(set { (get ()) with minor_heap_size = 256000 * 4 });
  (Lwt.async_exception_hook :=
     fun exn -> L.err (fun m -> m "async exn: %a" Error.pp (`Exn exn)));
  let inner () =
    let open Result.Infix in
    setup_logging ();
    let* config = Config.of_filename "./discopotty.toml" in
    let* ytdl = Ytdl.create () in
    let token = Config.token config in
    Lwt_main.run (Client.run ~handler:(handler config ytdl) token)
  in
  match inner () with
  | Ok () -> ()
  | Error error -> prerr_endline (Error.to_string error)
  | exception e -> prerr_endline Error.(of_exn e |> to_string)
