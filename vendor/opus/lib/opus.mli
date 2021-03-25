module Error: sig
  type t
  val to_string: t -> string
  val show: t -> string
  val pp: Format.formatter -> t -> unit
end

type nonrec 'a result = ('a, Error.t) result

type 'a handle

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

module CTL: sig
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

end

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type s16frame = (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
type f32frame = (int, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

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

  val ctl: t -> ([`enc] handle, 'a) CTL.t -> 'a result
end