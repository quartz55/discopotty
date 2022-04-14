(* open! Disco_core.Globals
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
     and waker = bool Lwt.t * bool Lwt.u

     let waker : unit -> waker = Lwt.task
     let make_silence ?n () = Audio_stream.n_silence_pipe ?n ()
     let create () = { state = Idle; silence = None; dead = false }
     let now () = Mtime_clock.elapsed_ns ()
     let playing t = match t.state with Playing s -> Some s | _ -> None
     let is_playing t = match t.state with Playing _ -> true | _ -> false

     let destroy_active t =
       match t.state with
       | Playing s ->
           Lwt.wakeup_later (snd s.waker) false;
           t.state <- Idle
       | Idle -> ()

     let stop = destroy_active

     let play ~s t =
       match t.state with
       | Playing os ->
           Lwt.wakeup_later (snd os.waker) false;
           if Option.is_none t.silence then t.silence <- Some (make_silence ());
           t.state <- Playing s
       | Idle -> t.state <- Playing s

     let destroy t = if not t.dead then t.dead <- true

     let run ~yield ~play ~stop t =
       let rec exhaust ?(i = 0) p =
         Lwt_pipe.read p >>= function
         | Some frame ->
             play (i + 1) frame >>= fun ok ->
             let i = i + 1 in
             if ok then exhaust ~i p else Lwt.return (`Yield i)
         | None -> Lwt.return (`Closed i)
       in
       let framelen = float Rtp._FRAME_LEN /. 1e3 in
       let schedule_next ?drift ?(frames = 1) () =
         let timeout = float frames *. framelen in
         let with_drift d =
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
       let last_tick = ref (now ()) in
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
         | `Tick ->
             let t = now () in
             L.trace (fun m ->
                 let delta = Int64.(t - !last_tick |> to_float) in
                 m "tick +%2fms" (delta /. 1e6));
             last_tick := t;
             Lwt.return_unit
       in
       let rec f' ?(drift = now ()) ?(i = 0) () =
         match (t.silence, t.state) with
         | _ when t.dead ->
             destroy_active t;
             Lwt.return_unit
         | Some s, st -> (
             exhaust ~i s >>= function
             | `Closed i ->
                 t.silence <- None;
                 (match st with Idle -> stop () | _ -> Lwt.return_unit)
                 >>= f' ~drift ~i
             | `Yield frames -> yield_till (schedule_next ~drift ~frames ()) >>= f'
             )
         | None, Idle ->
             (match !last_yield with
             | Some y ->
                 last_yield := None;
                 y
             | None -> yield t)
             >>= f'
         | None, Playing s -> (
             exhaust ~i s.rx >>= function
             | `Closed i ->
                 t.silence <- Some (make_silence ());
                 t.state <- Idle;
                 Lwt.wakeup_later (snd s.waker) true;
                 f' ~drift ~i ()
             | `Yield frames ->
                 yield_till (schedule_next ~drift ~frames ()) >>= fun () -> f' ())
       in
       f' ()
   end

   type t = { driver : Driver.t; evloop_tx : evloop_msg Lwt_pipe.Writer.t }
   and evloop_msg = Stop | Play of pcm_s16_frame Lwt_pipe.Reader.t * Driver.waker

   let create ?(burst = 15) out =
     let chan = Lwt_pipe.create () in
     let evloop_tx = Lwt_pipe.Writer.map ~f:(fun msg -> `Req msg) chan in
     let driver = Driver.create () in
     let yield driver =
       Lwt_pipe.read chan >|= Option.get_or ~default:`Poison >|= function
       | `Req (Play (s, waker)) ->
           let s = Lwt_pipe.Reader.filter_map ~f:encode s in
           Driver.play driver ~s:{ waker; rx = s }
       | `Req Stop -> Driver.stop driver
       | `Poison -> Driver.destroy driver
     in
     let play i frame = out (`Play frame) >|= fun r -> r && i < burst in
     let stop () = out `Stop >|= ignore in
     let f = Driver.run ~yield ~play ~stop driver in
     Lwt_pipe.keep chan f;
     Lwt_pipe.link_close chan ~after:evloop_tx;
     Lwt.on_termination f (fun () -> Lwt_pipe.close_nonblock chan);
     { driver; evloop_tx }

   let play ?(k = fun ~status:_ s -> Lwt_pipe.close_nonblock s) t stream =
     let ((wp, _) as waker) = Driver.waker () in
     Lwt.on_success wp (fun status -> k ~status stream);
     Lwt_pipe.write_exn t.evloop_tx (Play (stream, waker))

   let destroy t = Lwt_pipe.close_nonblock t.evloop_tx *)
