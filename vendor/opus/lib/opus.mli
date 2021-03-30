module Error: sig
  type t
  val to_string: t -> string
  val show: t -> string
  val pp: Format.formatter -> t -> unit
end

type nonrec 'a result = ('a, Error.t) result

type framesize = Opus_c.Types.Framesize.t

type bitrate = Opus_c.Types.Bitrate.t

type bandwidth = Opus_c.Types.Bandwidth.t

type application = Opus_c.Types.Application.t

type signal = Opus_c.Types.Signal.t

module CTL: sig
  type enc = [`enc]
  type dec = [`dec]
  type 'a both = [< `enc | `dec] as 'a

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
end

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type s16frame = (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32frame = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

module Encoder: sig
  type t

  type packet = [ `Packet of bigstring | `DTX ]

  val recommended_max_size: int

  val create:
    ?samplerate:int ->
    ?channels:[`mono | `stereo] ->
    ?application:application ->
    unit ->
    t result

  val size: t -> int

  val encode_blit:
    t ->
    ?pcm_off:int ->
    ?duration:int ->
    s16frame ->
    ?buf_off:int ->
    ?max_size:int ->
    bigstring ->
    packet result

  val encode:
    t ->
    ?off:int ->
    ?duration:int ->
    ?max_size:int ->
    s16frame ->
    packet result

  val encode_blit_float:
    t ->
    ?pcm_off:int ->
    ?duration:int ->
    f32frame ->
    ?buf_off:int ->
    ?max_size:int ->
    bigstring ->
    (packet) result

  val encode_float:
    t ->
    ?off:int ->
    ?duration:int ->
    ?max_size:int ->
    f32frame ->
    packet result

  val ctl: t -> ([`enc], 'a) CTL.t -> 'a result
end

module Decoder: sig
  type t

  val create:
    ?samplerate:int ->
    ?channels:[`mono | `stereo] ->
    unit ->
    t result

  val size: t -> int

  val decode_blit:
    t ->
    ?fec:bool ->
    ?buf_off:int ->
    ?buf_len:int ->
    bigstring ->
    ?pcm_off:int ->
    ?framesize:int ->
    s16frame ->
    s16frame result

  val decode:
    t ->
    ?fec:bool ->
    ?off:int ->
    ?len:int ->
    bigstring ->
    s16frame result

  val decode_blit_float:
    t ->
    ?fec:bool ->
    ?buf_off:int ->
    ?buf_len:int ->
    bigstring ->
    ?pcm_off:int ->
    ?framesize:int ->
    f32frame ->
    f32frame result

  val decode_float:
    t ->
    ?fec:bool ->
    ?off:int ->
    ?len:int ->
    bigstring ->
    f32frame result

  val ctl: t -> ([`dec], 'a) CTL.t -> 'a result
end