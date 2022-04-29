open Containers
open Eio.Std
module D = Disco
module Voice = Disco_voice
module Client = D.Client.Make (Voice.Manager)
module M = Disco_models
module Msg = M.Message
module L = (val Relog.logger ~namespace:"Discopotty" ())

let setup_logging () =
  let open Relog in
  let verbosity =
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
  let mtx = Mutex.create () in
  Logs.set_level ~all:true @@ Some Logs.Debug;
  let report src lvl ~over k msgf =
    let namespace = Logs.Src.name src in
    let module L = (val Relog.logger ~namespace ()) in
    let log =
      match lvl with
      | Logs.Error -> L.err
      | Warning -> L.warn
      | App | Info -> L.info
      | Debug -> L.trace
    in
    let b = Buffer.create 80 in
    let k _ =
      log (fun m -> m "%s" (Buffer.to_bytes b |> Bytes.unsafe_to_string));
      over ();
      k ()
    in
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    let ppf = Format.formatter_of_buffer b in
    Format.kfprintf k ppf fmt
  in
  let relog_logs_adapter = { Logs.report } in
  Logs.set_reporter relog_logs_adapter;
  let handler r =
    if Level.Infix.(Record.level r <= verbosity) then (
      Mutex.lock mtx;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock mtx)
        (fun () -> cli_fmter cli_fmt r))
    else ()
  in
  Sink.make handler |> Relog.Sink.set

let handler cfg client =
  let prefix = Config.prefix cfg in
  (* FIXME do we need eio specific support? (luv in this case) *)
  (* let intr = Atomic.make false in
     Sys.set_signal Sys.sigint
       (Sys.Signal_handle
          (fun _ ->
            if Atomic.get intr then (
              L.warn (fun m ->
                  m
                    "forcing shutdown (you might see exceptions related to the \
                     ongoing cleanup)");
              exit (-1))
            else if Atomic.compare_and_set intr false true then (
              L.info (fun m -> m "received SIGINT, gracefully shutting down");
              Client.disconnect client))); *)
  let voice = Client.voice client in
  function
  | Disco_core.Events.Message_create
      { content; channel_id; guild_id = Some guild_id; _ } -> (
      match Cmd.of_message ~prefix content with
      | None -> ()
      | Some ("ping", args) ->
          let msg = Msg.fmt "@{<b>pong@} %s" args in
          Client.send_message channel_id msg client
      | Some ("suicide", _args) ->
          let msg =
            Msg.fmt "⚠️ @{<b>@{<i>disconnecting by user request...@}@} 👋"
          in
          Client.send_message channel_id msg client;
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
            Msg.fmt "⚠️ Not supported yet, please provide a voice channel id"
          in
          Client.send_message channel_id msg client
      | Some ("join", vchan) -> (
          let vchan = M.Snowflake.of_string vchan in
          let call = Voice.Manager.get ~guild_id voice in
          try Voice.Call.join call ~channel_id:vchan
          with exn ->
            let msg =
              Msg.fmt "⚠️ Couldn't join voice channel: %s"
                (Printexc.to_string exn)
            in
            Client.send_message channel_id msg client)
      | Some ("leave", _) ->
          let call = Voice.Manager.get ~guild_id voice in
          Voice.Call.leave call
      | Some ("soundtest", "kiff") ->
          let call = Voice.Manager.get ~guild_id voice in
          Switch.run @@ fun sw ->
          let path = String.concat "/" [ Sys.getcwd (); "kiff.mp3" ] in
          let audio = Voice.Ffmpeg.process ~sw (`File path) in
          Voice.Call.play ~audio call
      | Some (other, _) ->
          let msg = Msg.fmt "🛑 @{<b>unsupported command@} @{<code>%s@}" other in
          Client.send_message channel_id msg client)
  | _ -> L.debug (fun m -> m "don't care")

let main env =
  Gc.(set { (get ()) with minor_heap_size = 256000 * 4 });
  Printexc.record_backtrace true;
  setup_logging ();
  let config = Config.of_filename "./discopotty.toml" in
  let token = Config.token config in
  try Client.run ~env ~handler:(handler config) token
  with e ->
    let bt, es = Printexc.(get_backtrace (), to_string e) in
    L.err (fun m -> m "fatal exception: %s@.%s" es bt);
    exit (-1)

let () =
  Eio_unix.Ctf.with_tracing "discopotty.ctf" @@ fun () ->
  Eio_luv.run (fun env -> main (env :> Eio.Stdenv.t))
