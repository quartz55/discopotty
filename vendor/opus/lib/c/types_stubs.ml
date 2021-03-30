module Def (F : Cstubs_structs.TYPE) = struct
  open Ctypes
  open F

  module Error = struct
    let t = int
    let alloc_fail = constant "OPUS_ALLOC_FAIL" int
    let invalid_state = constant "OPUS_INVALID_STATE" int
    let unimplemented = constant "OPUS_UNIMPLEMENTED" int
    let invalid_packet = constant "OPUS_INVALID_PACKET" int
    let internal_error = constant "OPUS_INTERNAL_ERROR" int
    let buffer_too_small = constant "OPUS_BUFFER_TOO_SMALL" int
    let bad_arg = constant "OPUS_BAD_ARG" int
    let ok = constant "OPUS_OK" int

    type t = [
     `Alloc_fail
    | `Invalid_state
    | `Unimplemented
    | `Invalid_packet
    | `Internal_error
    | `Buffer_too_small
    | `Bad_arg
    | `Unknown of int ]

    let of_c = function
    | e when e = ok -> raise (Invalid_argument "not an error")
    | e when e = alloc_fail -> `Alloc_fail
    | e when e = invalid_state -> `Invalid_state
    | e when e = unimplemented -> `Unimplemented
    | e when e = invalid_packet -> `Invalid_packet
    | e when e = internal_error -> `Internal_error
    | e when e = buffer_too_small -> `Buffer_too_small
    | e when e = bad_arg -> `Bad_arg
    | other -> `Unknown other

    let of_c_opt e =
    match of_c e with
    | err -> Some err
    | exception Invalid_argument _ -> None
  end

  let auto = constant "OPUS_AUTO" int

  module Application = struct
    let voip = constant "OPUS_APPLICATION_VOIP" int
    let audio = constant "OPUS_APPLICATION_AUDIO" int
    let restricted_lowedelay = constant "OPUS_APPLICATION_RESTRICTED_LOWDELAY" int

    type t = [
      | `Voip
      | `Audio
      | `Restricted_lowdelay
    ]

    let to_c = function
    | `Voip -> voip
    | `Audio -> audio
    | `Restricted_lowdelay -> restricted_lowedelay

    let of_c = function
    | n when n = voip -> `Voip
    | n when n = audio -> `Audio
    | n when n = restricted_lowedelay -> `Restricted_lowdelay
    | _ -> assert false
  end

  module Signal = struct
    let voice = constant "OPUS_SIGNAL_VOICE" int
    let music = constant "OPUS_SIGNAL_MUSIC" int

    type t = [
      | `Voice
      | `Music
    ]

    let of_c = function
    | n when n = voice -> `Voice
    | n when n = music -> `Music
    | _ -> assert false

    let to_c = function
    | `Voice -> voice
    | `Music -> music
  end

  module Bitrate = struct
    let max = constant "OPUS_BITRATE_MAX" int

    type t = [
      | `Max
      | `Bps of int
    ]

    let of_c = function
    | n when n = max -> `Max
    | n -> `Bps n

    let to_c = function
    | `Max -> max
    | `Bps bps -> bps
  end

  module Bandwidth = struct
    let narrowband = constant "OPUS_BANDWIDTH_NARROWBAND" int
    let mediumband = constant "OPUS_BANDWIDTH_MEDIUMBAND" int
    let wideband = constant "OPUS_BANDWIDTH_WIDEBAND" int
    let superwideband = constant "OPUS_BANDWIDTH_SUPERWIDEBAND" int
    let fullband = constant "OPUS_BANDWIDTH_FULLBAND" int

    type t = [
      | `Narrow
      | `Medium
      | `Wide
      | `Superwide
      | `Full
    ]

    let of_c = function
    | n when n = narrowband -> `Narrow 
    | n when n = mediumband -> `Medium 
    | n when n = wideband -> `Wide 
    | n when n = superwideband -> `Superwide 
    | n when n = fullband -> `Full 
    | _ -> assert false

    let to_c = function
    | `Narrow -> narrowband
    | `Medium -> mediumband
    | `Wide -> wideband
    | `Superwide -> superwideband
    | `Full -> fullband
  end

  module Framesize = struct
    let arg = constant "OPUS_FRAMESIZE_ARG" int
    let _2_5ms = constant "OPUS_FRAMESIZE_2_5_MS" int
    let _5ms = constant "OPUS_FRAMESIZE_5_MS" int
    let _10ms = constant "OPUS_FRAMESIZE_10_MS" int
    let _20ms = constant "OPUS_FRAMESIZE_20_MS" int
    let _40ms = constant "OPUS_FRAMESIZE_40_MS" int
    let _60ms = constant "OPUS_FRAMESIZE_60_MS" int
    let _80ms = constant "OPUS_FRAMESIZE_80_MS" int
    let _100ms = constant "OPUS_FRAMESIZE_100_MS" int
    let _120ms = constant "OPUS_FRAMESIZE_120_MS" int

    type t = [
      | `Arg
      | `Fs_2_5ms
      | `Fs_5ms
      | `Fs_10ms
      | `Fs_20ms
      | `Fs_40ms
      | `Fs_60ms
      | `Fs_80ms
      | `Fs_100ms
      | `Fs_120ms
    ]

    let to_c = function
    | `Arg -> arg
    | `Fs_2_5ms -> _2_5ms
    | `Fs_5ms -> _5ms
    | `Fs_10ms -> _10ms
    | `Fs_20ms -> _20ms
    | `Fs_40ms -> _40ms
    | `Fs_60ms -> _60ms
    | `Fs_80ms -> _80ms
    | `Fs_100ms -> _100ms
    | `Fs_120ms -> _120ms

    let of_c = function
    | n when n = arg -> `Arg
    | n when n = _2_5ms -> `Fs_2_5ms
    | n when n = _5ms -> `Fs_5ms
    | n when n = _10ms -> `Fs_10ms
    | n when n = _20ms -> `Fs_20ms
    | n when n = _40ms -> `Fs_40ms
    | n when n = _60ms -> `Fs_60ms
    | n when n = _80ms -> `Fs_80ms
    | n when n = _100ms -> `Fs_100ms
    | n when n = _120ms -> `Fs_120ms
    | _ -> assert false
  end

  module Ctl = struct
    let set_application_request = constant "OPUS_SET_APPLICATION_REQUEST" int
    let get_application_request = constant "OPUS_GET_APPLICATION_REQUEST" int
    let set_bitrate_request = constant "OPUS_SET_BITRATE_REQUEST" int
    let get_bitrate_request = constant "OPUS_GET_BITRATE_REQUEST" int
    let set_max_bandwidth_request = constant "OPUS_SET_MAX_BANDWIDTH_REQUEST" int
    let get_max_bandwidth_request = constant "OPUS_GET_MAX_BANDWIDTH_REQUEST" int
    let set_vbr_request = constant "OPUS_SET_VBR_REQUEST" int
    let get_vbr_request = constant "OPUS_GET_VBR_REQUEST" int
    let set_bandwidth_request = constant "OPUS_SET_BANDWIDTH_REQUEST" int
    let get_bandwidth_request = constant "OPUS_GET_BANDWIDTH_REQUEST" int
    let set_complexity_request = constant "OPUS_SET_COMPLEXITY_REQUEST" int
    let get_complexity_request = constant "OPUS_GET_COMPLEXITY_REQUEST" int
    let set_inband_fec_request = constant "OPUS_SET_INBAND_FEC_REQUEST" int
    let get_inband_fec_request = constant "OPUS_GET_INBAND_FEC_REQUEST" int
    let set_packet_loss_perc_request = constant "OPUS_SET_PACKET_LOSS_PERC_REQUEST" int
    let get_packet_loss_perc_request = constant "OPUS_GET_PACKET_LOSS_PERC_REQUEST" int
    let set_dtx_request = constant "OPUS_SET_DTX_REQUEST" int
    let get_dtx_request = constant "OPUS_GET_DTX_REQUEST" int
    let set_vbr_constraint_request = constant "OPUS_SET_VBR_CONSTRAINT_REQUEST" int
    let get_vbr_constraint_request = constant "OPUS_GET_VBR_CONSTRAINT_REQUEST" int
    let set_force_channels_request = constant "OPUS_SET_FORCE_CHANNELS_REQUEST" int
    let get_force_channels_request = constant "OPUS_GET_FORCE_CHANNELS_REQUEST" int
    let set_signal_request = constant "OPUS_SET_SIGNAL_REQUEST" int
    let get_signal_request = constant "OPUS_GET_SIGNAL_REQUEST" int
    let get_lookahead_request = constant "OPUS_GET_LOOKAHEAD_REQUEST" int
    let reset_state = constant "OPUS_RESET_STATE" int
    let get_sample_rate_request = constant "OPUS_GET_SAMPLE_RATE_REQUEST" int
    let get_final_range_request = constant "OPUS_GET_FINAL_RANGE_REQUEST" int
    let get_pitch_request = constant "OPUS_GET_PITCH_REQUEST" int
    let set_gain_request = constant "OPUS_SET_GAIN_REQUEST" int
    let get_gain_request = constant "OPUS_GET_GAIN_REQUEST" int
    let set_lsb_depth_request = constant "OPUS_SET_LSB_DEPTH_REQUEST" int
    let get_lsb_depth_request = constant "OPUS_GET_LSB_DEPTH_REQUEST" int
    let get_last_packet_duration_request = constant "OPUS_GET_LAST_PACKET_DURATION_REQUEST" int
    let set_expert_frame_duration_request = constant "OPUS_SET_EXPERT_FRAME_DURATION_REQUEST" int
    let get_expert_frame_duration_request = constant "OPUS_GET_EXPERT_FRAME_DURATION_REQUEST" int
    let set_prediction_disabled_request = constant "OPUS_SET_PREDICTION_DISABLED_REQUEST" int
    let get_prediction_disabled_request = constant "OPUS_GET_PREDICTION_DISABLED_REQUEST" int
    let set_phase_inversion_disabled_request = constant "OPUS_SET_PHASE_INVERSION_DISABLED_REQUEST" int
    let get_phase_inversion_disabled_request = constant "OPUS_GET_PHASE_INVERSION_DISABLED_REQUEST" int
    let get_in_dtx_request = constant "OPUS_GET_IN_DTX_REQUEST" int
  end

  include (struct
    type +'a handle = unit ptr
    let handle: 'a handle typ = ptr void
  end : sig
    type +'a handle
    val handle: 'a handle typ
  end
  )

  module Encoder = struct
    type t = [`enc] handle
    let t: t typ = handle
  end

  module Decoder = struct
    type t = [`dec] handle
    let t: t typ = handle
  end
end