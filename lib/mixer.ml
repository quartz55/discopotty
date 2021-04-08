open Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

type pcm_s16_frame =
  (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t

type pcm_f32_frame =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type silence_stream = bigstring Lwt_pipe.Reader.t

module Driver = struct
  type t = {
    mutable state : state;
    mutable silence : silence_stream option;
    mutable dead : bool;
  }

  and state = Idle | Playing of stream

  and stream = { waker : waker; rx : bigstring Lwt_pipe.Reader.t }

  and waker = unit Lwt.t * unit Lwt.u

  let waker : unit -> waker = Lwt.task

  let make_silence ?n () = Audio_stream.n_silence_pipe ?n ()

  let create () = { state = Idle; silence = None; dead = false }

  let now () = Mtime_clock.elapsed_ns ()

  let destroy_active t =
    match t.state with
    | Playing s ->
        Lwt_pipe.close_nonblock s.rx;
        Lwt.cancel (fst s.waker);
        t.state <- Idle
    | Idle -> ()

  let stop = destroy_active

  let play ~s t =
    match t.state with
    | Playing _ ->
        stop t;
        t.silence <- Some (make_silence ());
        t.state <- Playing s
    | Idle -> t.state <- Playing s

  let destroy t = if not t.dead then t.dead <- true

  let run ~yield ~play ~stop t =
    let rec exhaust ?(i = 0) p =
      Lwt_pipe.read p >>= function
      | Some frame ->
          play frame >>= fun ok ->
          if ok then exhaust ~i:(i + 1) p
          else if i = 0 then failwith "need to send at least 1 frame"
          else Lwt.return (`Yield i)
      | None -> Lwt.return (`Closed i)
    in
    let framelen = float Rtp._FRAME_LEN /. 1e3 in
    let schedule_next ?drift ?(frames = 1) () =
      let timeout = float frames *. framelen in
      let with_drift d =
        let d = Lazy.force d in
        let delta = Int64.(now () - d |> to_float) /. 1e9 in
        timeout -. Float.min delta timeout
      in
      let timeout =
        Option.map with_drift drift |> Option.get_or ~default:timeout
      in
      L.trace (fun m ->
          m "sent %d frames, next tick in %f seconds" frames timeout);
      Lwt_unix.sleep timeout >|= fun () -> `Tick
    in
    let last_yield = ref None in
    let rec yield_till tick =
      let y =
        match !last_yield with
        | Some y -> y
        | None ->
            let y = yield t in
            last_yield := Some y;
            y
      in
      Lwt.choose [ tick; (y >|= fun () -> `Yield) ] >>= function
      | `Yield when t.dead ->
          Lwt.cancel tick;
          Lwt.return_unit
      | `Yield ->
          last_yield := None;
          yield_till tick
      | `Tick -> Lwt.return_unit
    in
    let rec f' ?(drift = lazy (now ())) ?(i = 0) () =
      match (t.silence, t.state) with
      | _ when t.dead ->
          destroy_active t;
          Lwt.return_unit
      | Some s, st -> (
          exhaust s >>= function
          | `Closed n ->
              t.silence <- None;
              (match st with Idle -> stop () | _ -> Lwt.return_unit)
              >>= f' ~drift ~i:(i + n)
          | `Yield n ->
              let frames = i + n in
              yield_till (schedule_next ~drift ~frames ()) >>= fun () -> f' ())
      | None, Idle ->
          (match !last_yield with
          | Some y ->
              last_yield := None;
              y
          | None -> yield t)
          >>= fun () -> f' ()
      | None, Playing s -> (
          exhaust s.rx >>= function
          | `Closed n ->
              t.silence <- Some (make_silence ());
              t.state <- Idle;
              Lwt.wakeup_later (snd s.waker) ();
              f' ~drift ~i:(i + n) ()
          | `Yield n ->
              let frames = i + n in
              yield_till (schedule_next ~drift ~frames ()) >>= fun () -> f' ())
    in
    f' ()
end

type t = { evloop_tx : evloop_msg Lwt_pipe.Writer.t }

and evloop_msg = Stop | Play of pcm_s16_frame Lwt_pipe.Reader.t * Driver.waker

let create_opus_encoder ?(frametype = `s16) () =
  let open Opus in
  let open Result.Infix in
  let rec set_ctls ~l enc =
    match l with
    | ctl :: l -> Encoder.ctl enc ctl >>= fun () -> set_ctls ~l enc
    | [] -> Ok enc
  in
  Encoder.create ~samplerate:Rtp._SAMPLE_RATE ~channels:`stereo
    ~application:`Audio ()
  >>= (fun enc ->
        let shared =
          CTL.
            [
              Set_signal `Music;
              Set_bitrate `Max;
              Set_complexity 10;
              Set_packet_loss_perc 5;
              Set_complexity 10;
              Set_inband_FEC true;
              Set_DTX false;
            ]
        in
        let ctls =
          match frametype with
          | `s16 -> CTL.(Set_LSB_depth 16) :: shared
          | `f32 -> CTL.(Set_LSB_depth 24) :: shared
        in
        set_ctls ~l:ctls enc)
  |> Result.map_err (fun e ->
         `Msg (Printf.sprintf "opus error: %s" (Opus.Error.to_string e)))

let create ?(burst = 15) voice =
  let ( let+ ) = Result.( let+ ) in
  let chan = Lwt_pipe.create () in
  let evloop_tx = Lwt_pipe.Writer.map ~f:(fun msg -> `Req msg) chan in
  let+ encoder = create_opus_encoder () in
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
  let driver = Driver.create () in
  let yield driver =
    Lwt_pipe.read chan >|= Option.get_or ~default:`Poison >|= function
    | `Req (Play (s, waker)) ->
        let s = Lwt_pipe.Reader.filter_map ~f:encode s in
        Driver.play driver ~s:{ waker; rx = s }
    | `Req Stop -> Driver.stop driver
    | `Poison -> Driver.destroy driver
  in
  let curr = ref 0 in
  let play frame =
    if !curr < burst then (
      Voice.start_speaking voice >>= fun () ->
      Voice.send_rtp voice frame >|= fun () ->
      incr curr;
      true)
    else (
      curr := 0;
      Lwt.return false)
  in
  let stop () = Voice.stop_speaking voice in
  let f = Driver.run ~yield ~play ~stop driver in
  Lwt_pipe.keep chan f;
  Lwt_pipe.link_close chan ~after:evloop_tx;
  Lwt.on_termination f (fun () -> Lwt_pipe.close_nonblock chan);
  { evloop_tx }

let play ?k t stream =
  let ((wp, _) as waker) = Driver.waker () in
  let () = match k with Some f -> Lwt.on_success wp f | None -> () in
  Lwt_pipe.write_exn t.evloop_tx (Play (stream, waker))

let destroy t = Lwt_pipe.close_nonblock t.evloop_tx
