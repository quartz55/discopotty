open! Disco_core.Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())

exception Opus_error of Opus.Error.t

let get_opus_res_exn t =
  match t with Ok v -> v | Error err -> raise @@ Opus_error err

type frametype = [ `s16 | `f32 ]

let create_opus_encoder ?(frametype = `s16) () =
  let ( let+ ) res fn = fn (get_opus_res_exn res) in
  let open Opus in
  let rec set_ctls ~l enc =
    match l with
    | ctl :: l ->
        let+ () = Encoder.ctl enc ctl in
        set_ctls ~l enc
    | [] -> enc
  in
  let+ enc =
    Encoder.create ~samplerate:Rtp._SAMPLE_RATE ~channels:`stereo
      ~application:`Audio ()
  in
  let shared =
    CTL.
      [
        Set_signal `Music;
        Set_bitrate `Max;
        Set_complexity 10;
        Set_packet_loss_perc 5;
        Set_inband_FEC true;
        Set_DTX false;
      ]
  in
  let ctls =
    match frametype with
    | `s16 -> CTL.(Set_LSB_depth 16) :: shared
    | `f32 -> CTL.(Set_LSB_depth 24) :: shared
  in
  set_ctls ~l:ctls enc

let opt_of_opus_res = function
  | Ok `DTX ->
      L.err (fun m -> m "DTX????");
      None
  | Ok (`Packet p) -> Some p
  | Error e -> raise @@ Opus_error e

let opus_s16 = create_opus_encoder ~frametype:`s16 ()
let opus_f32 = create_opus_encoder ~frametype:`f32 ()

let s16_encoder () =
  let buf = Bigstringaf.create Rtp._VOICE_PACKET_MAX in
  fun frame ->
    Opus.Encoder.encode_blit opus_s16 ~duration:Rtp._FRAME_LEN frame buf
    |> opt_of_opus_res

let f32_encoder () =
  let buf = Bigstringaf.create Rtp._VOICE_PACKET_MAX in
  fun frame ->
    Opus.Encoder.encode_blit_float opus_f32 ~duration:Rtp._FRAME_LEN frame buf
    |> opt_of_opus_res

let silence_frame =
  [ '\xf8'; '\xff'; '\xfe' ] |> String.of_list
  |> Bigstringaf.of_string ~off:0 ~len:3

type t = { mutable src : bigstring Streaming.Source.t option }

let of_source src = { src = Some src }

(** Conservative default of 10 already taking into
 account the occasional packet loss that might occur *)
let silence_frames ?(n = 10) () =
  let src = Streaming.Source.generate ~len:n @@ fun _ -> silence_frame in
  of_source src

let of_raw_s16 src =
  let enc = s16_encoder () in
  Streaming.Source.filter_map enc src |> of_source

let of_raw_f32 src =
  let enc = f32_encoder () in
  Streaming.Source.filter_map enc src |> of_source

let read t =
  match t.src with
  | Some src -> (
      Streaming.Source.next src |> function
      | Some (v, src) ->
          t.src <- Some src;
          Some v
      | None -> None)
  | None -> None

let close t =
  match t.src with
  | Some src ->
      t.src <- None;
      Streaming.Source.dispose src
  | None -> ()
