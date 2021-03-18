open Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

type pcm_s16_frame =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

type pcm_f32_frame =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = { evloop_chan : evloop_msg Lwt_pipe.Writer.t }

and evloop_msg =
  | Detach of bool
  | Attach of Voice.t
  | Pause
  | Resume
  | Stop
  | Play of pcm_s16_frame Lwt_pipe.Reader.t

let create () =
  let chan = Lwt_pipe.create () in
  let tick () = Lwt_pipe.write chan `Tick >|= ignore in
  let frames_per_tick = 15 in
  let schedule_tick ?drift ?(n = 1) ?(timeout = float Rtp._FRAME_LEN /. 1e3) ()
      =
    Lwt.async (fun () ->
        let timeout = float n *. timeout in
        let timeout =
          Option.map
            (fun d ->
              timeout -. (Int64.(Mtime_clock.now_ns () - d |> to_float) /. 1e9))
            drift
          |> Option.get_or ~default:timeout
        in
        L.trace (fun m -> m "sent %d frames, next tick in %f seconds" n timeout);
        Lwt_unix.sleep timeout >>= tick)
  in
  let send_frames frames_p voice =
    match Lwt_pipe.is_closed frames_p with
    | true -> Lwt.return_none
    | false ->
        let rec f' = function
          | 0 -> Lwt.return_some frames_per_tick
          | n -> (
              Lwt_pipe.read frames_p >>= function
              | Some bs -> Voice.send_rtp voice bs >>= fun () -> f' (n - 1)
              | None -> Lwt.return_some (frames_per_tick - n))
        in
        Voice.start_speaking voice >>= fun () -> f' frames_per_tick
  in
  let evloop_chan = Lwt_pipe.Writer.map ~f:(fun msg -> `Req msg) chan in
  let voice = ref None in
  let stream = ref `Idle in
  let encoder =
    let open Opus in
    let open Result.Infix in
    Encoder.create ~samplerate:Rtp._SAMPLE_RATE ~channels:`stereo
      ~application:Audio ()
    >>= (fun enc ->
          Encoder.ctl enc (Set_signal `music) >>= fun () ->
          Encoder.ctl enc (Set_bitrate `max) >>= fun () ->
          Encoder.ctl enc (Set_complexity 10) >>= fun () ->
          Encoder.ctl enc (Set_LSB_depth 16) >>= fun () ->
          Encoder.ctl enc (Set_packet_loss_perc 5) >>= fun () ->
          Encoder.ctl enc (Set_complexity 10) >>= fun () ->
          Encoder.ctl enc (Set_inband_FEC true) >|= fun () -> enc)
    |> Result.get_lazy (fun e ->
           failwith @@ "opus error: " ^ Opus.Error.to_string e)
  in
  let encode =
    let pkt_buf = Bigstringaf.create Rtp._VOICE_PACKET_MAX in
    fun buf ->
      match
        Opus.Encoder.encode_blit encoder ~duration:Rtp._FRAME_LEN buf pkt_buf
      with
      | Ok `DTX ->
          L.err (fun m -> m "DTX!!!");
          None
      | Ok (`Packet p) -> Some p
      | Error e -> failwith ("opus error: " ^ Opus.Error.to_string e)
  in
  let last_tick = ref @@ Mtime_clock.elapsed_ns () in
  let rec evloop () =
    Lwt_pipe.read chan >|= Option.get_exn >>= function
    | `Tick ->
        let t = Mtime_clock.elapsed_ns () in
        L.trace (fun m -> m "time since last tick=%Ldns" Int64.(t - !last_tick));
        last_tick := t;
        !voice
        |> Option.map (fun voice ->
               let t1 = Mtime_clock.now_ns () in
               match !stream with
               | `Idle -> evloop ()
               | `Silence p -> (
                   send_frames p voice >>= function
                   | Some n ->
                       schedule_tick ~drift:t1 ~n ();
                       evloop ()
                   | None ->
                       stream := `Idle;
                       Voice.stop_speaking voice >>= evloop)
               | `Audio pcm -> (
                   let enc = Lwt_pipe.Reader.filter_map ~f:encode pcm in
                   send_frames enc voice >>= function
                   | Some n ->
                       Lwt_pipe.close_nonblock enc;
                       schedule_tick ~drift:t1 ~n ();
                       evloop ()
                   | None ->
                       stream := `Silence (Audio_stream.n_silence_pipe ());
                       Lwt_pipe.close_nonblock enc;
                       Lwt.async tick;
                       evloop ()))
        |> Option.get_lazy evloop
    | `Req (Attach n_voice) ->
        voice := Some n_voice;
        evloop ()
    | `Req (Play audio) ->
        let () =
          match !stream with
          | `Silence p -> Lwt_pipe.close_nonblock p
          | `Audio p -> Lwt_pipe.close_nonblock p
          | `Idle -> Lwt.async tick
        in
        stream := `Audio audio;
        evloop ()
    | `Req _msg -> evloop ()
    | `Poison -> Lwt.return_unit
  in
  let f = evloop () in
  Lwt_pipe.keep chan f;
  Lwt.on_termination f (fun () -> Lwt_pipe.close_nonblock chan);
  { evloop_chan }

let attach t voice = Lwt_pipe.write_exn t.evloop_chan (Attach voice)

let play t stream = Lwt_pipe.write_exn t.evloop_chan (Play stream)
