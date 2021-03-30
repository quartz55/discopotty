module C = Opus_c
module T = Opus_c.Types

open Ctypes

let mem = `Ocaml

module Error = struct
  type t = T.Error.t

  let wrap_lazy fn e = match T.Error.of_c_opt e with
  | Some err -> Error err
  | None -> Ok (fn ())

  let wrap a = wrap_lazy (fun () -> a)
  let to_string = C.Error.to_string
  let to_string = function
  | `Alloc_fail -> to_string T.Error.alloc_fail
  | `Invalid_state -> to_string T.Error.invalid_state
  | `Unimplemented -> to_string T.Error.unimplemented
  | `Invalid_packet -> to_string T.Error.invalid_packet
  | `Internal_error -> to_string T.Error.internal_error
  | `Buffer_too_small -> to_string T.Error.buffer_too_small
  | `Bad_arg -> to_string T.Error.bad_arg
  | `Unknown other -> "unknown error: " ^ (string_of_int other)

  let show = to_string
  let pp fmt t = Format.fprintf fmt "%s" (show t)
end

type nonrec 'a result = ('a, Error.t) result

type framesize = T.Framesize.t

type bitrate = T.Bitrate.t

type bandwidth = T.Bandwidth.t

type application = T.Application.t

type signal = T.Signal.t

module CTL = struct
  type enc = [`enc]
  type dec = [`dec]
  type 'a both = [< `enc | `dec] as 'a
  type 'a handle = 'a T.handle

  type ('handle, 'res) t =
  (* generic *)
  | Reset_state: (_ both, unit) t
  | Get_final_range: (_ both, int) t
  | Get_bandwidth: (_ both, [`Auto | bandwidth]) t
  | Get_sample_rate: (_ both, int) t
  | Set_phase_inversion_disabled: bool -> (_ both, unit) t
  | Get_phase_inversion_disabled: (_ both, bool) t
  | Get_in_DTX: (_ both, bool) t
  (* encoder *)
  | Set_complexity: int -> (enc, unit) t
  | Get_complexity: (enc, int) t
  | Set_bitrate: [`Auto | bitrate] -> (enc, unit) t
  | Get_bitrate: (enc, [`Auto | bitrate]) t
  | Set_VBR: bool -> (enc, unit) t
  | Get_VBR: (enc, bool) t
  | Set_VBR_constraint: bool -> (enc, unit) t
  | Get_VBR_constraint: (enc, bool) t
  | Set_force_channels: [`Auto | `Mono | `Stereo] -> (enc, unit) t
  | Get_force_channels: (enc, [`Auto | `Mono | `Stereo]) t
  | Set_max_bandwidth: bandwidth -> (enc, unit) t
  | Get_max_bandwidth: (enc, bandwidth) t
  | Set_bandwidth: [`Auto | bandwidth] -> (enc, unit) t
  | Set_signal: [`Auto | signal] -> (enc, unit) t
  | Get_signal: (enc, [`Auto | signal]) t
  | Set_application: application -> (enc, unit) t
  | Get_application: (enc, application) t
  | Get_lookahead: (enc, int) t
  | Set_inband_FEC: bool -> (enc, unit) t
  | Get_inband_FEC: (enc, bool) t
  | Set_packet_loss_perc: int -> (enc, unit) t
  | Get_packet_loss_perc: (enc, int) t
  | Set_DTX: bool -> (enc, unit) t
  | Get_DTX: (enc, bool) t
  | Set_LSB_depth: int -> (enc, unit) t
  | Get_LSB_depth: (enc, int) t
  | Set_expert_frame_duration: framesize -> (enc, unit) t
  | Get_expert_frame_duration: (enc, framesize) t
  | Set_prediction_disabled: bool -> (enc, unit) t
  | Get_prediction_disabled: (enc, bool) t
  (* decoder *)
  | Set_gain: int -> (dec, unit) t
  | Get_gain: (dec, int) t
  | Get_last_packet_duration: (dec, int) t
  | Get_pitch: (dec, int) t

  let _code: type a b. (a, b) t -> int32 =
   fun t ->
    let o = match t with
    (* generic *)
    | Reset_state -> T.Ctl.reset_state
    | Get_final_range -> T.Ctl.get_final_range_request
    | Get_bandwidth -> T.Ctl.get_bandwidth_request
    | Get_sample_rate -> T.Ctl.get_sample_rate_request
    | Set_phase_inversion_disabled _ -> T.Ctl.set_phase_inversion_disabled_request
    | Get_phase_inversion_disabled -> T.Ctl.get_phase_inversion_disabled_request
    | Get_in_DTX -> T.Ctl.get_in_dtx_request
    (* encoder *)
    | Set_complexity _ -> T.Ctl.set_complexity_request
    | Get_complexity -> T.Ctl.get_complexity_request
    | Set_bitrate _ -> T.Ctl.set_bitrate_request
    | Get_bitrate -> T.Ctl.get_bitrate_request
    | Set_VBR _ -> T.Ctl.set_vbr_request
    | Get_VBR -> T.Ctl.get_vbr_request
    | Set_VBR_constraint _ -> T.Ctl.set_vbr_constraint_request
    | Get_VBR_constraint -> T.Ctl.get_vbr_constraint_request
    | Set_force_channels _ -> T.Ctl.set_force_channels_request
    | Get_force_channels -> T.Ctl.get_force_channels_request
    | Set_max_bandwidth _ -> T.Ctl.set_max_bandwidth_request
    | Get_max_bandwidth -> T.Ctl.get_max_bandwidth_request
    | Set_bandwidth _ -> T.Ctl.set_bandwidth_request
    | Set_signal _ -> T.Ctl.set_signal_request
    | Get_signal -> T.Ctl.get_signal_request
    | Set_application _ -> T.Ctl.set_application_request
    | Get_application -> T.Ctl.get_application_request
    | Get_lookahead -> T.Ctl.get_lookahead_request
    | Set_inband_FEC _ -> T.Ctl.set_inband_fec_request
    | Get_inband_FEC -> T.Ctl.get_inband_fec_request
    | Set_packet_loss_perc _ -> T.Ctl.set_packet_loss_perc_request
    | Get_packet_loss_perc -> T.Ctl.get_packet_loss_perc_request
    | Set_DTX _ -> T.Ctl.set_dtx_request
    | Get_DTX -> T.Ctl.get_dtx_request
    | Set_LSB_depth _ -> T.Ctl.set_lsb_depth_request
    | Get_LSB_depth -> T.Ctl.get_lsb_depth_request
    | Set_expert_frame_duration _ -> T.Ctl.set_expert_frame_duration_request
    | Get_expert_frame_duration -> T.Ctl.get_expert_frame_duration_request
    | Set_prediction_disabled _ -> T.Ctl.set_prediction_disabled_request
    | Get_prediction_disabled -> T.Ctl.get_prediction_disabled_request
    (* decoder *)
    | Set_gain _ -> T.Ctl.set_gain_request
    | Get_gain -> T.Ctl.get_gain_request
    | Get_last_packet_duration -> T.Ctl.get_last_packet_duration_request
    | Get_pitch -> T.Ctl.get_pitch_request
    in
    Int32.of_int o

  module R = struct
    type _ arg =
    | Arg: 'a * 'a conv -> 'a arg
    and _ conv =
    | Conv: ('a -> int32) * (int32 -> 'a) -> 'a conv
  
    type ('i, 'o) def =
    | Set: 'i arg -> ('i, unit) def
    | Get: 'o conv -> (unit, 'o) def

    let fn_0 ~code ~handle name =
      let fn = Foreign.foreign name (T.handle @-> int32_t @-> returning int) in
      fn handle code |> Error.wrap ()
  
    let fn_1: type i o. code:int32 -> handle:'a handle -> string -> (i, o) def -> o result =
     fun ~code ~handle name def ->
      match def with
      | Set (Arg (a, Conv (ser, _))) ->
        let fn = Foreign.foreign name (T.handle @-> int32_t @-> int32_t @-> returning int) in
        fn handle code (ser a) |> Error.wrap ()
      | Get (Conv (_, des)) ->
        let fn = Foreign.foreign name (T.handle @-> int32_t @-> ptr int32_t @-> returning int) in
        let v = allocate int32_t 0l in
        fn handle code v |> Error.wrap (des !@v)

      let (%>) a b x = b @@ a x

      let set arg = Set arg
      let get conv = Get conv
      let conv: type i. (i -> int) -> (int -> i) -> i conv =
        fun ser des -> Conv (ser %> Int32.of_int, Int32.to_int %> des)
      let arg conv a = Arg (a, conv)

      let bool = conv Bool.to_int (function | 0 -> false | 1 -> true | _ -> assert false)
      let int = conv (fun i -> i) (fun i -> i)

      let max_bandwidth =
        conv T.Bandwidth.to_c T.Bandwidth.of_c

      let bandwidth =
        let to_int = function | #bandwidth as b -> T.Bandwidth.to_c b | `Auto -> T.auto in
        let of_int = function | n when n = T.auto -> `Auto | n -> T.Bandwidth.of_c n in
        conv to_int of_int

      let bitrate =
        let to_int = function
        | `Auto -> T.auto
        | #bitrate as br -> T.Bitrate.to_c br
        in
        let of_int = function
        | n when n = T.auto -> `Auto
        | br -> T.Bitrate.of_c br
        in
        conv to_int of_int

      let signal =
        let to_int = function
        | `Auto -> T.auto
        | #signal as s -> T.Signal.to_c s
        in
        let of_int = function
        | s when s = T.auto -> `Auto
        | s -> T.Signal.of_c s
        in
        conv to_int of_int

      let channels =
        let to_int = function
        | `Auto -> T.auto
        | `Mono -> 1
        | `Stereo -> 2
        in
        let of_int = function
        | n when n = T.auto -> `Auto
        | 1 -> `Mono
        | 2 -> `Stereo
        | _ -> assert false
        in
        conv to_int of_int

      let application = conv T.Application.to_c T.Application.of_c

      let framesize = conv T.Framesize.to_c T.Framesize.of_c
  end

  let request: type a b. ctl_fn:string -> a handle -> (a, b) t -> b result
    = fun ~ctl_fn handle req ->
    let code = _code req in
    match req with
    (* generic *)
    | Reset_state -> R.fn_0 ~code ~handle ctl_fn
    | Get_final_range -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Get_bandwidth -> R.fn_1 ~code ~handle ctl_fn R.(get bandwidth)
    | Get_sample_rate -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Set_phase_inversion_disabled b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_phase_inversion_disabled -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    | Get_in_DTX -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    (* encoder *)
    | Set_complexity c -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg int c)
    | Get_complexity -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Set_bitrate br -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bitrate br)
    | Get_bitrate -> R.fn_1 ~code ~handle ctl_fn R.(get bitrate)
    | Set_VBR b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_VBR -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    | Set_VBR_constraint b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_VBR_constraint -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    | Set_force_channels ch -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg channels ch)
    | Get_force_channels -> R.fn_1 ~code ~handle ctl_fn R.(get channels)
    | Set_max_bandwidth bw -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg max_bandwidth bw)
    | Get_max_bandwidth -> R.fn_1 ~code ~handle ctl_fn R.(get max_bandwidth)
    | Set_bandwidth bw -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bandwidth bw)
    | Set_signal s -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg signal s)
    | Get_signal -> R.fn_1 ~code ~handle ctl_fn R.(get signal)
    | Set_application a -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg application a)
    | Get_application -> R.fn_1 ~code ~handle ctl_fn R.(get application)
    | Get_lookahead -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Set_inband_FEC b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_inband_FEC -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    | Set_packet_loss_perc plp -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg int plp)
    | Get_packet_loss_perc -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Set_DTX b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_DTX -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    | Set_LSB_depth d -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg int d)
    | Get_LSB_depth -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Set_expert_frame_duration fs -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg framesize fs)
    | Get_expert_frame_duration -> R.fn_1 ~code ~handle ctl_fn R.(get framesize)
    | Set_prediction_disabled b -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg bool b)
    | Get_prediction_disabled -> R.fn_1 ~code ~handle ctl_fn R.(get bool)
    (* encoder *)
    | Set_gain g -> R.fn_1 ~code ~handle ctl_fn R.(set @@ arg int g)
    | Get_gain -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Get_last_packet_duration -> R.fn_1 ~code ~handle ctl_fn R.(get int)
    | Get_pitch -> R.fn_1 ~code ~handle ctl_fn R.(get int)
end

module BA = Bigarray
module BA1 = Bigarray.Array1
type bigstring = (char, BA.int8_unsigned_elt, BA.c_layout) BA1.t
type s16frame = (int, BA.int16_signed_elt, BA.c_layout) BA1.t
type f32frame = (float, BA.float32_elt, BA.c_layout) BA1.t

module Encoder = struct
  type encoder = T.Encoder.t

  type t = {
    enc: enc;
    samplerate: int;
    channels: int;
    app: application;
  }
  and enc = | Own of encoder | C of encoder * gc
  and gc = unit ptr

  type packet = [ `Packet of bigstring | `DTX ]

  let recommended_max_size = 4000

  let _enc = function | Own e | C (e, _) -> e

  let create ?(samplerate=48000) ?(channels=`stereo) ?(application=`Audio) () =
    let channels = match channels with | `stereo -> 2 | `mono -> 1 in
    let app = T.Application.to_c application in
    let err, enc = match mem with
    | `C ->
      let err = allocate int (-1) in
      let enc = C.Encoder.create Int32.(of_int samplerate) channels app err in
      let gc =
        allocate_n ~finalise:(fun _ -> C.Encoder.destroy enc)
        ~count:0 void
      in
      !@err, C (enc, gc)
    | `Ocaml -> 
      let enc =
        allocate_n ~count:(C.Encoder.get_size channels) uchar
        |> coerce (ptr uchar) (ptr void)
      in
      let err = C.Encoder.init enc Int32.(of_int samplerate) channels app in
      err, Own (Obj.magic enc)
    in
    Error.wrap enc err
    |> Result.map (fun enc -> {
      enc;
      samplerate;
      channels;
      app=application;
    })
  
  let size { channels; _ } = C.Encoder.get_size channels
    
  let _encode_blit:
    type a b.
    t ->
    ?pcm_off:int ->
    ?duration:int ->
    (a, b, BA.c_layout) BA1.t ->
    ?buf_off:int ->
    ?max_size:int ->
    bigstring ->
    packet result =
    fun t ?(pcm_off=0) ?(duration=20) pcm
      ?(buf_off=0) ?max_size buf ->
    let pcm = BA1.sub pcm pcm_off (BA1.dim pcm - pcm_off) in
    let buf = BA1.sub buf buf_off (BA1.dim buf - buf_off) in
    let max_size = match max_size with
      | Some size when size > (BA1.dim buf) ->
        raise (Invalid_argument "max_size is bigger than available buf space")
      | Some s -> s
      | None -> BA1.dim buf
    in
    let fs = t.samplerate / 1000 * duration in
    if BA1.dim pcm < (fs * t.channels) then
      raise (Invalid_argument "given pcm frame does not have enough samples");
    let buf_p =
      (bigarray_start array1 buf) |>
      coerce (ptr char) (ptr uchar)
    in
    let len = match BA1.kind pcm with
      | BA.Int16_signed -> C.Encoder.encode (_enc t.enc) (bigarray_start array1 pcm) fs buf_p max_size
      | BA.Float32 -> C.Encoder.encode_float (_enc t.enc) (bigarray_start array1 pcm) fs buf_p max_size
      | _ ->
        print_endline "!!!FATAL!!! invalid opus pcm buffer type (must be s16 or f32)";
        exit (-1)
    in
    match len with
    | n when n < 0 -> Error (T.Error.of_c n)
    | n when n <= 2 -> Ok `DTX
    | n -> Ok (`Packet (BA1.sub buf 0 n))

  let encode_blit t ?pcm_off ?duration pcm ?buf_off ?max_size buf =
    _encode_blit t ?pcm_off ?duration pcm ?buf_off ?max_size buf

  let encode t ?off ?duration ?(max_size=recommended_max_size) pcm =
    let buf = BA1.create BA.char BA.c_layout max_size in
    _encode_blit t ?pcm_off:off ?duration pcm buf
  
  let encode_blit_float t ?pcm_off ?duration pcm ?buf_off ?max_size buf =
    _encode_blit t ?pcm_off ?duration pcm ?buf_off ?max_size buf

  let encode_float t ?off ?duration ?(max_size=recommended_max_size) pcm =
    let buf = BA1.create BA.char BA.c_layout max_size in
    _encode_blit t ?pcm_off:off ?duration pcm buf
  
  let ctl t req = CTL.request ~ctl_fn:"opus_encoder_ctl" (_enc t.enc) req
end

module Decoder = struct
  type decoder = T.Decoder.t

  type t = {
    dec: dec;
    samplerate: int;
    channels: int;
  }
  and dec = | Own of decoder | C of decoder * gc
  and gc = unit ptr

  let _dec = function | Own e | C (e, _) -> e

  let create ?(samplerate=48000) ?(channels=`stereo) () =
    let channels = match channels with | `stereo -> 2 | `mono -> 1 in
    let err, dec = match mem with
    | `C ->
      let err = allocate int (-1) in
      let dec = C.Decoder.create Int32.(of_int samplerate) channels err in
      let gc =
        allocate_n ~finalise:(fun _ -> C.Decoder.destroy dec)
        ~count:0 void
      in
      !@err, C (dec, gc)
    | `Ocaml -> 
      let dec =
        allocate_n ~count:(C.Decoder.get_size channels) uchar
        |> coerce (ptr uchar) (ptr void)
      in
      let err = C.Decoder.init dec Int32.(of_int samplerate) channels in
      err, Own (Obj.magic dec)
    in
    Error.wrap dec err
    |> Result.map (fun dec -> {
      dec;
      samplerate;
      channels;
    })
  
  let size { channels; _ } = C.Decoder.get_size channels

  let _decode_blit:
    type a b.
    t ->
    ?fec:bool ->
    ?buf_off:int ->
    ?buf_len:int ->
    bigstring ->
    ?pcm_off:int ->
    ?framesize:int ->
    (a, b, BA.c_layout) BA1.t ->
    (a, b, BA.c_layout) BA1.t result =
    fun t ?(fec=false) ?(buf_off=0) ?buf_len buf ?(pcm_off=0) ?(framesize=120) pcm ->
    let pcm = BA1.sub pcm pcm_off (BA1.dim pcm - pcm_off) in
    let buf = BA1.sub buf buf_off (match buf_len with | None -> (BA1.dim buf - buf_off) | Some l -> l) in
    let fs = t.samplerate / 1000 * framesize in
    if BA1.dim pcm < (fs * t.channels) then
      raise (Invalid_argument "given pcm frame does not have enough samples");
    let buf_p =
      (bigarray_start array1 buf) |>
      coerce (ptr char) (ptr uchar)
    in
    let len = Int32.of_int @@ BA1.dim buf in
    let len = match BA1.kind pcm with
      | BA.Int16_signed -> C.Decoder.decode (_dec t.dec) buf_p len (bigarray_start array1 pcm) fs fec
      | BA.Float32 -> C.Decoder.decode_float (_dec t.dec) buf_p len (bigarray_start array1 pcm) fs fec
      | _ ->
        print_endline "!!!FATAL!!! invalid opus pcm buffer type (must be s16 or f32)";
        exit (-1)
    in
    match len with
    | n when n < 0 -> Error (T.Error.of_c n)
    | n -> Ok (BA1.sub pcm 0 (n * t.channels))

  let decode_blit t ?fec ?buf_off ?buf_len buf ?pcm_off ?framesize pcm =
    _decode_blit t ?fec ?buf_off ?buf_len buf ?pcm_off ?framesize pcm

  let decode t ?fec ?off ?len buf =
    let framesize = 120 in
    let pcm = BA1.create BA.int16_signed BA.c_layout (t.samplerate / 1000 * framesize * t.channels) in
    _decode_blit t ?fec ?buf_off:off ?buf_len:len buf ~framesize pcm

  let decode_blit_float t ?fec ?buf_off ?buf_len buf ?pcm_off ?framesize pcm =
    _decode_blit t ?fec ?buf_off ?buf_len buf ?pcm_off ?framesize pcm

  let decode_float t ?fec ?off ?len buf =
    let framesize = 120 in
    let pcm = BA1.create BA.float32 BA.c_layout (t.samplerate / 1000 * framesize * t.channels) in
    _decode_blit t ?fec ?buf_off:off ?buf_len:len buf ~framesize pcm

  let ctl t req = CTL.request ~ctl_fn:"opus_decoder_ctl" (_dec t.dec) req
end
