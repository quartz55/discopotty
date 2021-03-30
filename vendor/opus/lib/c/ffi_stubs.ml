module T = Opus_c_types

module Def (F : Ctypes.FOREIGN) = struct
  open Ctypes
  open F
  module Types = Opus_c_types

  module Error = struct
    let to_string = foreign "opus_strerror" (T.Error.t @-> returning string)
  end

  module Encoder = struct
    let get_size = foreign "opus_encoder_get_size" (int @-> returning int)
    let init = foreign "opus_encoder_init" (ptr void @-> int32_t @-> int @-> int @-> returning T.Error.t)
    let create = foreign "opus_encoder_create" (int32_t @-> int @-> int @-> ptr T.Error.t @-> returning T.Encoder.t)
    let destroy = foreign "opus_encoder_destroy" (T.Encoder.t @-> returning void)
  
    let encode = foreign "opus_encode" (T.Encoder.t @-> ptr int16_t @-> int @-> ptr uchar @-> int @-> returning int)
    let encode_float = foreign "opus_encode_float" (T.Encoder.t @-> ptr float @-> int @-> ptr uchar @-> int @-> returning int)
  end

  module Decoder = struct
    let get_size = foreign "opus_decoder_get_size" (int @-> returning int)
    let init = foreign "opus_decoder_init" (ptr void @-> int32_t @-> int @-> returning T.Error.t)
    let create = foreign "opus_decoder_create" (int32_t @-> int @-> ptr T.Error.t  @-> returning T.Decoder.t)
    let destroy = foreign "opus_decoder_destroy" (T.Decoder.t @-> returning void)
  
    let decode = foreign "opus_decode" (T.Decoder.t @-> ptr uchar @-> int32_t @-> ptr int16_t @-> int @-> bool @-> returning int)
    let decode_float = foreign "opus_decode_float" (T.Decoder.t @-> ptr uchar @-> int32_t @-> ptr float @-> int @-> bool @-> returning int)
  end
end
