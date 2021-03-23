let%c () = header {|
#include <opus.h>
#include <opus_multistream.h>
|}

open Ctypes

module C = struct
  let app_VOIP = [%c constant "OPUS_APPLICATION_VOIP" camlint]
  let app_AUDIO = [%c constant "OPUS_APPLICATION_AUDIO" camlint]
  let app_RESTRICTED_LOWEDELAY =
    [%c constant "OPUS_APPLICATION_RESTRICTED_LOWDELAY" camlint]
  
  let _AUTO = [%c constant "OPUS_AUTO" camlint]

  let signal_VOICE = [%c constant "OPUS_SIGNAL_VOICE" camlint]
  let signal_MUSIC = [%c constant "OPUS_SIGNAL_MUSIC" camlint]
  let br_MAX = [%c constant "OPUS_BITRATE_MAX" camlint]
  let ctl_SET_APPLICATION_REQUEST = [%c constant "OPUS_SET_APPLICATION_REQUEST" camlint]
  let ctl_GET_APPLICATION_REQUEST = [%c constant "OPUS_GET_APPLICATION_REQUEST" camlint]
  let ctl_SET_BITRATE_REQUEST = [%c constant "OPUS_SET_BITRATE_REQUEST" camlint]
  let ctl_GET_BITRATE_REQUEST = [%c constant "OPUS_GET_BITRATE_REQUEST" camlint]
  let ctl_SET_MAX_BANDWIDTH_REQUEST = [%c constant "OPUS_SET_MAX_BANDWIDTH_REQUEST" camlint]
  let ctl_GET_MAX_BANDWIDTH_REQUEST = [%c constant "OPUS_GET_MAX_BANDWIDTH_REQUEST" camlint]
  let ctl_SET_VBR_REQUEST = [%c constant "OPUS_SET_VBR_REQUEST" camlint]
  let ctl_GET_VBR_REQUEST = [%c constant "OPUS_GET_VBR_REQUEST" camlint]
  let ctl_SET_BANDWIDTH_REQUEST = [%c constant "OPUS_SET_BANDWIDTH_REQUEST" camlint]
  let ctl_GET_BANDWIDTH_REQUEST = [%c constant "OPUS_GET_BANDWIDTH_REQUEST" camlint]
  let ctl_SET_COMPLEXITY_REQUEST = [%c constant "OPUS_SET_COMPLEXITY_REQUEST" camlint]
  let ctl_GET_COMPLEXITY_REQUEST = [%c constant "OPUS_GET_COMPLEXITY_REQUEST" camlint]
  let ctl_SET_INBAND_FEC_REQUEST = [%c constant "OPUS_SET_INBAND_FEC_REQUEST" camlint]
  let ctl_GET_INBAND_FEC_REQUEST = [%c constant "OPUS_GET_INBAND_FEC_REQUEST" camlint]
  let ctl_SET_PACKET_LOSS_PERC_REQUEST = [%c constant "OPUS_SET_PACKET_LOSS_PERC_REQUEST" camlint]
  let ctl_GET_PACKET_LOSS_PERC_REQUEST = [%c constant "OPUS_GET_PACKET_LOSS_PERC_REQUEST" camlint]
  let ctl_SET_DTX_REQUEST = [%c constant "OPUS_SET_DTX_REQUEST" camlint]
  let ctl_GET_DTX_REQUEST = [%c constant "OPUS_GET_DTX_REQUEST" camlint]
  let ctl_SET_VBR_CONSTRAINT_REQUEST = [%c constant "OPUS_SET_VBR_CONSTRAINT_REQUEST" camlint]
  let ctl_GET_VBR_CONSTRAINT_REQUEST = [%c constant "OPUS_GET_VBR_CONSTRAINT_REQUEST" camlint]
  let ctl_SET_FORCE_CHANNELS_REQUEST = [%c constant "OPUS_SET_FORCE_CHANNELS_REQUEST" camlint]
  let ctl_GET_FORCE_CHANNELS_REQUEST = [%c constant "OPUS_GET_FORCE_CHANNELS_REQUEST" camlint]
  let ctl_SET_SIGNAL_REQUEST = [%c constant "OPUS_SET_SIGNAL_REQUEST" camlint]
  let ctl_GET_SIGNAL_REQUEST = [%c constant "OPUS_GET_SIGNAL_REQUEST" camlint]
  let ctl_GET_LOOKAHEAD_REQUEST = [%c constant "OPUS_GET_LOOKAHEAD_REQUEST" camlint]
  let ctl_RESET_STATE = [%c constant "OPUS_RESET_STATE" camlint]
  let ctl_GET_SAMPLE_RATE_REQUEST = [%c constant "OPUS_GET_SAMPLE_RATE_REQUEST" camlint]
  let ctl_GET_FINAL_RANGE_REQUEST = [%c constant "OPUS_GET_FINAL_RANGE_REQUEST" camlint]
  let ctl_GET_PITCH_REQUEST = [%c constant "OPUS_GET_PITCH_REQUEST" camlint]
  let ctl_SET_GAIN_REQUEST = [%c constant "OPUS_SET_GAIN_REQUEST" camlint]
  let ctl_GET_GAIN_REQUEST = [%c constant "OPUS_GET_GAIN_REQUEST" camlint]
  let ctl_SET_LSB_DEPTH_REQUEST = [%c constant "OPUS_SET_LSB_DEPTH_REQUEST" camlint]
  let ctl_GET_LSB_DEPTH_REQUEST = [%c constant "OPUS_GET_LSB_DEPTH_REQUEST" camlint]
  let ctl_GET_LAST_PACKET_DURATION_REQUEST = [%c constant "OPUS_GET_LAST_PACKET_DURATION_REQUEST" camlint]
  let ctl_SET_EXPERT_FRAME_DURATION_REQUEST = [%c constant "OPUS_SET_EXPERT_FRAME_DURATION_REQUEST" camlint]
  let ctl_GET_EXPERT_FRAME_DURATION_REQUEST = [%c constant "OPUS_GET_EXPERT_FRAME_DURATION_REQUEST" camlint]
  let ctl_SET_PREDICTION_DISABLED_REQUEST = [%c constant "OPUS_SET_PREDICTION_DISABLED_REQUEST" camlint]
  let ctl_GET_PREDICTION_DISABLED_REQUEST = [%c constant "OPUS_GET_PREDICTION_DISABLED_REQUEST" camlint]
  let ctl_SET_PHASE_INVERSION_DISABLED_REQUEST = [%c constant "OPUS_SET_PHASE_INVERSION_DISABLED_REQUEST" camlint]
  let ctl_GET_PHASE_INVERSION_DISABLED_REQUEST = [%c constant "OPUS_GET_PHASE_INVERSION_DISABLED_REQUEST" camlint]
  let ctl_GET_IN_DTX_REQUEST = [%c constant "OPUS_GET_IN_DTX_REQUEST" camlint]
end

module Error = struct
  let%c t = int

  let _ALLOC_FAIL = [%c constant "OPUS_ALLOC_FAIL" camlint]
  let _INVALID_STATE = [%c constant "OPUS_INVALID_STATE" camlint]
  let _UNIMPLEMENTED = [%c constant "OPUS_UNIMPLEMENTED" camlint]
  let _INVALID_PACKET = [%c constant "OPUS_INVALID_PACKET" camlint]
  let _INTERNAL_ERROR = [%c constant "OPUS_INTERNAL_ERROR" camlint]
  let _BUFFER_TOO_SMALL = [%c constant "OPUS_BUFFER_TOO_SMALL" camlint]
  let _BAD_ARG = [%c constant "OPUS_BAD_ARG" camlint]
  let _OK = [%c constant "OPUS_OK" camlint]

  external to_string: t -> string = "opus_strerror"

  type t = [
     `Alloc_fail
    | `Invalid_state
    | `Unimplemented
    | `Invalid_packet
    | `Internal_error
    | `Buffer_too_small
    | `Bad_arg
    | `Unknown of int ]

  let of_int = function
    | e when e = _OK -> raise (Invalid_argument "not an error")
    | e when e = _ALLOC_FAIL -> `Alloc_fail
    | e when e = _INVALID_STATE -> `Invalid_state
    | e when e = _UNIMPLEMENTED -> `Unimplemented
    | e when e = _INVALID_PACKET -> `Invalid_packet
    | e when e = _INTERNAL_ERROR -> `Internal_error
    | e when e = _BUFFER_TOO_SMALL -> `Buffer_too_small
    | e when e = _BAD_ARG -> `Bad_arg
    | other -> `Unknown other

  let wrap_lazy fn = function
  | e when e = _OK -> Ok (fn ())
  | e when e = _ALLOC_FAIL -> Error `Alloc_fail
  | e when e = _INVALID_STATE -> Error `Invalid_state
  | e when e = _UNIMPLEMENTED -> Error `Unimplemented
  | e when e = _INVALID_PACKET -> Error `Invalid_packet
  | e when e = _INTERNAL_ERROR -> Error `Internal_error
  | e when e = _BUFFER_TOO_SMALL -> Error `Buffer_too_small
  | e when e = _BAD_ARG -> Error `Bad_arg
  | other -> Error (`Unknown other)

  let wrap a = wrap_lazy (fun () -> a)

  let to_string = function
  | `Alloc_fail -> to_string _ALLOC_FAIL
  | `Invalid_state -> to_string _INVALID_STATE
  | `Unimplemented -> to_string _UNIMPLEMENTED
  | `Invalid_packet -> to_string _INVALID_PACKET
  | `Internal_error -> to_string _INTERNAL_ERROR
  | `Buffer_too_small -> to_string _BUFFER_TOO_SMALL
  | `Bad_arg -> to_string _BAD_ARG
  | `Unknown other -> "unknown error: " ^ (string_of_int other)

  let show = to_string
  let pp fmt t = Format.fprintf fmt "%s" (show t)
end

type nonrec 'a result = ('a, Error.t) result

type 'a handle = unit ptr
let handle_t: 'a handle typ = ptr void
let%c enc_handle: [`enc] handle typ = ptr void
let%c dec_handle: [`dec] handle typ = ptr void

type bandwidth = [
  | `Narrow
  | `Medium
  | `Wide
  | `Superwide
  | `Full
]

type framesize = [
  | `from_arg
  | `fs_2_5ms
  | `fs_5ms
  | `fs_10ms
  | `fs_20ms
  | `fs_40ms
  | `fs_60ms
  | `fs_80ms
  | `fs_100ms
  | `fs_120ms
]

type application =
  | Voip
  | Audio
  | Restricted_lowdelay

module CTL = struct
  external opus_encoder_ctl: enc_handle -> int32_t -> int32_t ptr -> Error.t = "opus_encoder_ctl"
  external opus_decoder_ctl: dec_handle -> int32_t -> int32_t ptr -> Error.t = "opus_decoder_ctl"

  type enc = [`enc]
  type dec = [`dec]
  type 'a both = [< `enc | `dec] as 'a

  type ('handle, 'res) t =
  (* generic *)
  | Reset_state: (_ both handle, unit) t
  | Get_final_range: (_ both handle, int) t
  | Get_bandwidth: (_ both handle, [`auto | bandwidth]) t
  | Get_sample_rate: (_ both handle, int) t
  | Set_phase_inversion_disabled: bool -> (_ both handle, unit) t
  | Get_phase_inversion_disabled: (_ both handle, bool) t
  | Get_in_DTX: (_ both handle, bool) t
  (* encoder *)
  | Set_complexity: int -> (enc handle, unit) t
  | Get_complexity: (enc handle, int) t
  | Set_bitrate: [`auto | `max | `bps of int] -> (enc handle, unit) t
  | Get_bitrate: (enc handle, [`auto | `max | `bps of int]) t
  | Set_VBR: bool -> (enc handle, unit) t
  | Get_VBR: (enc handle, bool) t
  | Set_VBR_constraint: bool -> (enc handle, unit) t
  | Get_VBR_constraint: (enc handle, bool) t
  | Set_force_channels: [`auto | `mono | `stereo] -> (enc handle, unit) t
  | Get_force_channels: (enc handle, [`auto | `mono | `stereo]) t
  | Set_max_bandwidth: bandwidth -> (enc handle, unit) t
  | Get_max_bandwidth: (enc handle, bandwidth) t
  | Set_bandwidth: [`auto | bandwidth] -> (enc handle, unit) t
  | Set_signal: [`auto | `voice | `music] -> (enc handle, unit) t
  | Get_signal: (enc handle, [`auto | `voice | `music]) t
  | Set_application: application -> (enc handle, unit) t
  | Get_application: (enc handle, application) t
  | Get_lookahead: (enc handle, int) t
  | Set_inband_FEC: bool -> (enc handle, unit) t
  | Get_inband_FEC: (enc handle, bool) t
  | Set_packet_loss_perc: int -> (enc handle, unit) t
  | Get_packet_loss_perc: (enc handle, int) t
  | Set_DTX: bool -> (enc handle, unit) t
  | Get_DTX: (enc handle, bool) t
  | Set_LSB_depth: int -> (enc handle, unit) t
  | Get_LSB_depth: (enc handle, int) t
  | Set_expert_frame_duration: framesize -> (enc handle, unit) t
  | Get_expert_frame_duration: (enc handle, framesize) t
  | Set_prediction_disabled: bool -> (enc handle, unit) t
  | Get_prediction_disabled: (enc handle, bool) t
  (* decoder *)

  let _code: type a b. (a, b) t -> int32 =
   fun t ->
    let o = match t with
    (* generic *)
    | Reset_state -> C.ctl_RESET_STATE
    | Get_final_range -> C.ctl_GET_FINAL_RANGE_REQUEST
    | Get_bandwidth -> C.ctl_GET_BANDWIDTH_REQUEST
    | Get_sample_rate -> C.ctl_GET_SAMPLE_RATE_REQUEST
    | Set_phase_inversion_disabled _ -> C.ctl_SET_PHASE_INVERSION_DISABLED_REQUEST
    | Get_phase_inversion_disabled -> C.ctl_GET_PHASE_INVERSION_DISABLED_REQUEST
    | Get_in_DTX -> C.ctl_GET_IN_DTX_REQUEST
    (* encoder *)
    | Set_complexity _ -> C.ctl_SET_COMPLEXITY_REQUEST
    | Get_complexity -> C.ctl_GET_COMPLEXITY_REQUEST
    | Set_bitrate _ -> C.ctl_SET_BITRATE_REQUEST
    | Get_bitrate -> C.ctl_GET_BITRATE_REQUEST
    | Set_VBR _ -> C.ctl_SET_VBR_REQUEST
    | Get_VBR -> C.ctl_GET_VBR_REQUEST
    | Set_VBR_constraint _ -> C.ctl_SET_VBR_CONSTRAINT_REQUEST
    | Get_VBR_constraint -> C.ctl_GET_VBR_CONSTRAINT_REQUEST
    | Set_force_channels _ -> C.ctl_SET_FORCE_CHANNELS_REQUEST
    | Get_force_channels -> C.ctl_GET_FORCE_CHANNELS_REQUEST
    | Set_max_bandwidth _ -> C.ctl_SET_MAX_BANDWIDTH_REQUEST
    | Get_max_bandwidth -> C.ctl_GET_MAX_BANDWIDTH_REQUEST
    | Set_bandwidth _ -> C.ctl_SET_BANDWIDTH_REQUEST
    | Set_signal _ -> C.ctl_SET_SIGNAL_REQUEST
    | Get_signal -> C.ctl_GET_SIGNAL_REQUEST
    | Set_application _ -> C.ctl_SET_APPLICATION_REQUEST
    | Get_application -> C.ctl_GET_APPLICATION_REQUEST
    | Get_lookahead -> C.ctl_GET_LOOKAHEAD_REQUEST
    | Set_inband_FEC _ -> C.ctl_SET_INBAND_FEC_REQUEST
    | Get_inband_FEC -> C.ctl_GET_INBAND_FEC_REQUEST
    | Set_packet_loss_perc _ -> C.ctl_SET_PACKET_LOSS_PERC_REQUEST
    | Get_packet_loss_perc -> C.ctl_GET_PACKET_LOSS_PERC_REQUEST
    | Set_DTX _ -> C.ctl_SET_DTX_REQUEST
    | Get_DTX -> C.ctl_GET_DTX_REQUEST
    | Set_LSB_depth _ -> C.ctl_SET_LSB_DEPTH_REQUEST
    | Get_LSB_depth -> C.ctl_GET_LSB_DEPTH_REQUEST
    | Set_expert_frame_duration _ -> C.ctl_SET_EXPERT_FRAME_DURATION_REQUEST
    | Get_expert_frame_duration -> C.ctl_GET_EXPERT_FRAME_DURATION_REQUEST
    | Set_prediction_disabled _ -> C.ctl_SET_PREDICTION_DISABLED_REQUEST
    | Get_prediction_disabled -> C.ctl_GET_PREDICTION_DISABLED_REQUEST
    in
    Int32.of_int o
  
  let request: type a b. ctl_fn:string -> a handle -> (a, b) t -> b result
    = fun ~ctl_fn handle req ->
    let code = _code req in
    match req with
    (* generic *)
    | Reset_state ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> returning int) in
      fn handle code |> Error.wrap ()
    | Get_phase_inversion_disabled ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let b = allocate int32_t 0l in
      fn handle code b |> Error.wrap (match !@b with | 0l -> false | 1l -> true | _ -> assert false)
    (* encoder *)
    | Set_complexity c ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      fn handle code (Int32.of_int c) |> Error.wrap ()
    | Get_complexity ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let c = allocate int32_t 0l in
      fn handle code c |> Error.wrap (Int32.to_int !@c)
    | Set_bitrate br ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      let br = match br with
        | `auto -> C._AUTO
        | `max -> C.br_MAX
        | `bps br -> br
      in
      fn handle code (Int32.of_int br) |> Error.wrap ()
    | Get_bitrate ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let br = allocate int32_t 0l in
      fn handle code br |> Error.wrap_lazy (fun () ->
        match Int32.to_int !@br with
        | n when n = C._AUTO -> `auto
        | n when n = C.br_MAX -> `max
        | o -> `bps o)
    | Set_packet_loss_perc plp ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      fn handle code (Int32.of_int plp) |> Error.wrap ()
    | Get_packet_loss_perc ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let plp = allocate int32_t 0l in
      fn handle code plp |> Error.wrap (Int32.to_int !@plp)
    | Set_signal s ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      let s = match s with
      | `auto -> C._AUTO
      | `voice -> C.signal_VOICE
      | `music -> C.signal_MUSIC
      in
      fn handle code (Int32.of_int s) |> Error.wrap ()
    | Get_signal ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let s = allocate int32_t 0l in
      fn handle code s
      |> Error.wrap_lazy (fun () ->
        match Int32.to_int !@s with
        | s when s = C._AUTO -> `auto
        | s when s = C.signal_VOICE -> `voice
        | s when s = C.signal_MUSIC -> `music
        | _ -> assert false)
    | Set_inband_FEC b ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      fn handle code (Int32.of_int @@ Bool.to_int b) |> Error.wrap ()
    | Get_inband_FEC ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let b = allocate int32_t 0l in
      fn handle code b |> Error.wrap (match !@b with | 0l -> false | 1l -> true | _ -> assert false)
    | Set_LSB_depth d ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      fn handle code (Int32.of_int d) |> Error.wrap ()
    | Get_LSB_depth ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let d = allocate int32_t 0l in
      fn handle code d |> Error.wrap (Int32.to_int !@d)
    | Set_DTX b ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> int32_t @-> returning int) in
      fn handle code (Int32.of_int @@ Bool.to_int b) |> Error.wrap ()
    | Get_DTX ->
      let fn = Foreign.foreign ctl_fn (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
      let b = allocate int32_t 0l in
      fn handle code b |> Error.wrap (match !@b with | 0l -> false | 1l -> true | _ -> assert false)
    | _ -> assert false
end

module BA = Bigarray
module BA1 = Bigarray.Array1
type bigstring = (char, BA.int8_unsigned_elt, BA.c_layout) BA1.t
type s16frame = (int, BA.int16_signed_elt, BA.c_layout) BA1.t
type f32frame = (int, BA.float32_elt, BA.c_layout) BA1.t

module Encoder = struct
  type encoder = [`enc] handle
  let%c t = ptr void
  external opus_encoder_size: int -> int = "opus_encoder_get_size"
  external opus_encoder_init: t -> int32_t -> int -> int -> Error.t = "opus_encoder_init"
  external opus_encoder_create: int32_t -> int -> int -> Error.t ptr -> t = "opus_encoder_create"
  external opus_encoder_destroy: t -> void = "opus_encoder_destroy"

  external opus_encode: t -> pcm:int16_t ptr -> fs:int -> buf:uchar ptr -> max:int -> int = "opus_encode"
  external opus_encode_float: t -> pcm:float ptr -> fs:int -> buf:uchar ptr -> max:int -> int = "opus_encode_float"

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

  let own = true

  let _enc = function | Own e | C (e, _) -> e

  let create ?(samplerate=48000) ?(channels=`stereo) ?(application=Audio) () =
    let c = match channels with | `stereo -> 2 | `mono -> 1 in
    let app = match application with
      | Voip -> C.app_VOIP
      | Audio -> C.app_AUDIO
      | Restricted_lowdelay -> C.app_RESTRICTED_LOWEDELAY
    in
    let err, enc = match own with
    | false ->
      let err = allocate int (-1) in
      let enc = opus_encoder_create Int32.(of_int samplerate) c app err in
      let gc =
        allocate_n ~finalise:(fun _ -> opus_encoder_destroy enc)
        ~count:0 void
      in
      !@err, C (enc, gc)
    | true -> 
      let enc =
        allocate_n ~count:(opus_encoder_size c) uchar
        |> coerce (ptr uchar) (ptr void)
      in
      let err = opus_encoder_init enc Int32.(of_int samplerate) c app in
      err, Own enc
    in
    Error.wrap enc err
    |> Result.map (fun enc -> {
      enc;
      samplerate;
      channels=c;
      app=application;
    })
  
  let size { channels; _ } = opus_encoder_size channels
    
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
      | BA.Int16_signed -> opus_encode (_enc t.enc) ~pcm:(bigarray_start array1 pcm) ~fs ~buf:buf_p ~max:max_size
      | BA.Float32 -> opus_encode_float (_enc t.enc) ~pcm:(bigarray_start array1 pcm) ~fs ~buf:buf_p ~max:max_size
      | _ ->
        print_endline "!!!FATAL!!! invalid opus pcm buffer type (must be s16 or f32)";
        exit (-1)
    in
    match len with
    | n when n < 0 -> Error (Error.of_int n)
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
