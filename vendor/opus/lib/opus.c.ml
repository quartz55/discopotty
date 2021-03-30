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

  let bw_NARROWBAND = [%c constant "OPUS_BANDWIDTH_NARROWBAND" camlint]
  let bw_MEDIUMBAND = [%c constant "OPUS_BANDWIDTH_MEDIUMBAND" camlint]
  let bw_WIDEBAND = [%c constant "OPUS_BANDWIDTH_WIDEBAND" camlint]
  let bw_SUPERWIDEBAND = [%c constant "OPUS_BANDWIDTH_SUPERWIDEBAND" camlint]
  let bw_FULLBAND = [%c constant "OPUS_BANDWIDTH_FULLBAND" camlint]

  let fs_ARG = [%c constant "OPUS_FRAMESIZE_ARG" camlint]
  let fs_2_5_MS = [%c constant "OPUS_FRAMESIZE_2_5_MS" camlint]
  let fs_5_MS = [%c constant "OPUS_FRAMESIZE_5_MS" camlint]
  let fs_10_MS = [%c constant "OPUS_FRAMESIZE_10_MS" camlint]
  let fs_20_MS = [%c constant "OPUS_FRAMESIZE_20_MS" camlint]
  let fs_40_MS = [%c constant "OPUS_FRAMESIZE_40_MS" camlint]
  let fs_60_MS = [%c constant "OPUS_FRAMESIZE_60_MS" camlint]
  let fs_80_MS = [%c constant "OPUS_FRAMESIZE_80_MS" camlint]
  let fs_100_MS = [%c constant "OPUS_FRAMESIZE_100_MS" camlint]
  let fs_120_MS = [%c constant "OPUS_FRAMESIZE_120_MS" camlint]

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
  | `arg
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
  (* we don't use these directly, but it's 
     good for checking at cstubs compile time *)
  external opus_encoder_ctl: enc_handle -> int32_t -> int32_t ptr -> Error.t = "opus_encoder_ctl" [@@ocaml.warning "-32"]
  external opus_decoder_ctl: dec_handle -> int32_t -> int32_t ptr -> Error.t = "opus_decoder_ctl" [@@ocaml.warning "-32"]

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
  | Set_gain: int -> (dec handle, unit) t
  | Get_gain: (dec handle, int) t
  | Get_last_packet_duration: (dec handle, int) t
  | Get_pitch: (dec handle, int) t

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
    (* decoder *)
    | Set_gain _ -> C.ctl_SET_GAIN_REQUEST
    | Get_gain -> C.ctl_GET_GAIN_REQUEST
    | Get_last_packet_duration -> C.ctl_GET_LAST_PACKET_DURATION_REQUEST
    | Get_pitch -> C.ctl_GET_PITCH_REQUEST
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
      let fn = Foreign.foreign name (handle_t @-> int32_t @-> returning int) in
      fn handle code |> Error.wrap ()
  
    let fn_1: type i o. code:int32 -> handle:'a handle -> string -> (i, o) def -> o result =
     fun ~code ~handle name def ->
      match def with
      | Set (Arg (a, Conv (ser, _))) ->
        let fn = Foreign.foreign name (handle_t @-> int32_t @-> int32_t @-> returning int) in
        fn handle code (ser a) |> Error.wrap ()
      | Get (Conv (_, des)) ->
        let fn = Foreign.foreign name (handle_t @-> int32_t @-> ptr int32_t @-> returning int) in
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

      let bw_to_int = function
      | `Narrow -> C.bw_NARROWBAND
      | `Medium -> C.bw_MEDIUMBAND
      | `Wide -> C.bw_WIDEBAND
      | `Superwide -> C.bw_SUPERWIDEBAND
      | `Full -> C.bw_FULLBAND

      let bw_of_int = function
      | n when n = C.bw_NARROWBAND -> `Narrow 
      | n when n = C.bw_MEDIUMBAND -> `Medium 
      | n when n = C.bw_WIDEBAND -> `Wide 
      | n when n = C.bw_SUPERWIDEBAND -> `Superwide 
      | n when n = C.bw_FULLBAND -> `Full 
      | _ -> assert false

      let max_bandwidth =
        conv bw_to_int bw_of_int

      let bandwidth =
        let to_int = function | #bandwidth as b -> bw_to_int b | `auto -> C._AUTO in
        let of_int = function | n when n = C._AUTO -> `auto | n -> bw_of_int n in
        conv to_int of_int

      let bitrate =
        let to_int = function
        | `auto -> C._AUTO
        | `max -> C.br_MAX
        | `bps br -> br
        in
        let of_int = function
        | n when n = C._AUTO -> `auto
        | n when n = C.br_MAX -> `max
        | o -> `bps o
        in
        conv to_int of_int

      let signal =
        let to_int = function
        | `auto -> C._AUTO
        | `voice -> C.signal_VOICE
        | `music -> C.signal_MUSIC
        in
        let of_int = function
        | s when s = C._AUTO -> `auto
          | s when s = C.signal_VOICE -> `voice
          | s when s = C.signal_MUSIC -> `music
          | _ -> assert false
        in
        conv to_int of_int

      let channels =
        let to_int = function
        | `auto -> C._AUTO
        | `mono -> 1
        | `stereo -> 2
        in
        let of_int = function
        | n when n = C._AUTO -> `auto
        | 1 -> `mono
        | 2 -> `stereo
        | _ -> assert false
        in
        conv to_int of_int

      let application =
        let to_int = function
        | Voip -> C.app_VOIP
        | Audio -> C.app_AUDIO
        | Restricted_lowdelay -> C.app_RESTRICTED_LOWEDELAY
        in
        let of_int = function
        | n when n = C.app_VOIP -> Voip
        | n when n = C.app_AUDIO -> Audio
        | n when n = C.app_RESTRICTED_LOWEDELAY -> Restricted_lowdelay
        | _ -> assert false
        in
        conv to_int of_int

      let framesize =
        let to_int = function
        | `arg -> C.fs_ARG
        | `fs_2_5ms -> C.fs_2_5_MS
        | `fs_5ms -> C.fs_5_MS
        | `fs_10ms -> C.fs_10_MS
        | `fs_20ms -> C.fs_20_MS
        | `fs_40ms -> C.fs_40_MS
        | `fs_60ms -> C.fs_60_MS
        | `fs_80ms -> C.fs_80_MS
        | `fs_100ms -> C.fs_100_MS
        | `fs_120ms -> C.fs_120_MS
        in
        let of_int = function
        | n when n = C.fs_ARG -> `arg
        | n when n = C.fs_2_5_MS -> `fs_2_5ms
        | n when n = C.fs_5_MS -> `fs_5ms
        | n when n = C.fs_10_MS -> `fs_10ms
        | n when n = C.fs_20_MS -> `fs_20ms
        | n when n = C.fs_40_MS -> `fs_40ms
        | n when n = C.fs_60_MS -> `fs_60ms
        | n when n = C.fs_80_MS -> `fs_80ms
        | n when n = C.fs_100_MS -> `fs_100ms
        | n when n = C.fs_120_MS -> `fs_120ms
        | _ -> assert false
        in
        conv to_int of_int
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
  type encoder = [`enc] handle
  let%c t = ptr void
  external opus_encoder_get_size: int -> int = "opus_encoder_get_size"
  external opus_encoder_init: t -> fs:int32_t -> channels:int -> app:int -> Error.t = "opus_encoder_init"
  external opus_encoder_create: fs:int32_t -> channels:int -> app:int -> Error.t ptr -> t = "opus_encoder_create"
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

  let _enc = function | Own e | C (e, _) -> e

  let create ?(samplerate=48000) ?(channels=`stereo) ?(application=Audio) () =
    let channels = match channels with | `stereo -> 2 | `mono -> 1 in
    let app = match application with
      | Voip -> C.app_VOIP
      | Audio -> C.app_AUDIO
      | Restricted_lowdelay -> C.app_RESTRICTED_LOWEDELAY
    in
    let err, enc = match own with
    | false ->
      let err = allocate int (-1) in
      let enc = opus_encoder_create ~fs:Int32.(of_int samplerate) ~channels ~app err in
      let gc =
        allocate_n ~finalise:(fun _ -> opus_encoder_destroy enc)
        ~count:0 void
      in
      !@err, C (enc, gc)
    | true -> 
      let enc =
        allocate_n ~count:(opus_encoder_get_size channels) uchar
        |> coerce (ptr uchar) (ptr void)
      in
      let err = opus_encoder_init enc ~fs:Int32.(of_int samplerate) ~channels ~app in
      err, Own enc
    in
    Error.wrap enc err
    |> Result.map (fun enc -> {
      enc;
      samplerate;
      channels;
      app=application;
    })
  
  let size { channels; _ } = opus_encoder_get_size channels
    
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

module Decoder = struct
  type decoder = [`dec] handle
  let%c t = ptr void
  external opus_decoder_get_size: int -> int = "opus_decoder_get_size"
  external opus_decoder_init: t -> fs:int32_t -> channels:int -> Error.t = "opus_decoder_init"
  external opus_decoder_create: fs:int32_t -> channels:int -> Error.t ptr -> t = "opus_decoder_create"
  external opus_decoder_destroy: t -> void = "opus_decoder_destroy"

  external opus_decode: t -> buf:uchar ptr -> len:int32_t -> pcm:int16_t ptr -> fs:int -> fec:bool-> int = "opus_decode"
  external opus_decode_float: t -> buf:uchar ptr -> len:int32_t -> pcm:float ptr -> fs:int -> fec:bool -> int = "opus_decode_float"

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
    let err, dec = match own with
    | false ->
      let err = allocate int (-1) in
      let dec = opus_decoder_create ~fs:Int32.(of_int samplerate) ~channels err in
      let gc =
        allocate_n ~finalise:(fun _ -> opus_decoder_destroy dec)
        ~count:0 void
      in
      !@err, C (dec, gc)
    | true -> 
      let dec =
        allocate_n ~count:(opus_decoder_get_size channels) uchar
        |> coerce (ptr uchar) (ptr void)
      in
      let err = opus_decoder_init dec ~fs:Int32.(of_int samplerate) ~channels in
      err, Own dec
    in
    Error.wrap dec err
    |> Result.map (fun dec -> {
      dec;
      samplerate;
      channels;
    })
  
  let size { channels; _ } = opus_decoder_get_size channels

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
      | BA.Int16_signed -> opus_decode (_dec t.dec) ~buf:buf_p ~len ~pcm:(bigarray_start array1 pcm) ~fs ~fec
      | BA.Float32 -> opus_decode_float (_dec t.dec) ~buf:buf_p ~len ~pcm:(bigarray_start array1 pcm) ~fs ~fec
      | _ ->
        print_endline "!!!FATAL!!! invalid opus pcm buffer type (must be s16 or f32)";
        exit (-1)
    in
    match len with
    | n when n < 0 -> Error (Error.of_int n)
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
