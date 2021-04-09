open Globals

let _SAMPLE_RATE = 48000

let _CHANNELS = 2

let _FRAME_LEN = 20

let _FRAME_SIZE = _SAMPLE_RATE / 1000 * _FRAME_LEN

(** Maximum packet size for a voice packet.
 Set below Ethernet MTU to avoid fragmentation/rejection. *)
let _VOICE_PACKET_MAX = 1460

(* https://tools.ietf.org/html/rfc7587#section-4.2 *)
let ts_incr = Uint32.of_int @@ _FRAME_SIZE

module Sync = struct
  type t = uint16 * uint32

  let add_uint16_safe a b =
    let open Uint16 in
    if compare (max_int - b) a < 0 then a - (max_int - b) else a + b

  let add_uint32_safe a b =
    let open Uint32 in
    if compare (max_int - b) a < 0 then a - (max_int - b) else a + b

  let make () =
    let seq = Random.(run (int Int.(2 ** 16))) |> Uint16.of_int in
    let ts = Random.int64 Int64.(2L ** 32L) |> Uint32.of_int64 in
    (seq, ts)

  let seq = fst

  let ts = snd

  let tick (seq, ts) =
    (add_uint16_safe seq Uint16.one, add_uint32_safe ts ts_incr)
end

type t = {
  udp : Udp_connection.t;
  mutable crypt : Udp_connection.crypt;
  mutable sync : Sync.t;
}

let make ~udp ~crypt = { udp; crypt; sync = Sync.make () }

let ssrc { udp; _ } = udp.ssrc

let set_crypt t crypt = t.crypt <- crypt

let send_packet t bs =
  let seq, ts = t.sync in
  t.sync <- Sync.tick t.sync;
  Udp_connection.send_voice_packet ~crypt:t.crypt ~seq ~ts ~audio:bs t.udp

let destroy t = Udp_connection.destroy t.udp
