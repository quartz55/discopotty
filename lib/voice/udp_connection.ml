open! Disco_core.Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Packet = struct
  module P = struct
    open Angstrom

    let is_num = function '0' .. '9' -> true | _ -> false

    let is_null = function '\x00' -> true | _ -> false

    module Ip = struct
      let packet =
        let* () =
          BE.any_uint16 >>= function
          | 2 -> return ()
          | d -> fail (Printf.sprintf "expected response type (0x2), got %X" d)
        in
        let* len = BE.any_uint16 in
        let* body_i = pos in
        let* ssrc = BE.any_int32 >>| Int32.to_int in
        let* addr_buf = take 64 in
        let addr_p = take_while1 @@ Fun.(is_null %> not) in
        let* addr =
          match
            Angstrom.parse_string ~consume:Angstrom.Consume.Prefix addr_p
              addr_buf
          with
          | Ok a -> return a
          | Error e -> fail e
        in
        let* port = BE.any_uint16 in
        let+ () =
          end_of_input
          *> (pos >>= function
              | n when n - body_i = len -> return ()
              | n ->
                  fail
                    (Printf.sprintf
                       "expected body length of %d bytes, got %d bytes" len n))
        in
        (ssrc, addr, port)
    end
  end

  module Ip = struct
    let of_bigstring bs =
      Angstrom.parse_bigstring ~consume:Angstrom.Consume.All P.Ip.packet bs

    let write ~ssrc f =
      Faraday.BE.write_uint16 f 0x1;
      Faraday.BE.write_uint16 f 70;
      Faraday.BE.write_uint32 f (Int32.of_int ssrc);
      Faraday.write_bytes f (Bytes.create 66)
  end

  module Keep_alive = struct
    let write ~ssrc f = Faraday.BE.write_uint32 f (Int32.of_int ssrc)

    let interval = 5.
  end

  module RTP = struct
    let header_len = 12

    let write_header ~seq ~ts ~ssrc f =
      let s = Faraday.pending_bytes f in
      Faraday.write_uint8 f 0x80;
      Faraday.write_uint8 f 0x78;
      Faraday.BE.write_uint16 f Uint16.(to_int seq);
      Faraday.BE.write_uint32 f Uint32.(to_int32 ts);
      Faraday.BE.write_uint32 f Int32.(of_int ssrc);
      assert (Faraday.pending_bytes f - s = header_len)
  end
end

let ip_discovery ?(retries = 3) ?(timeout = 1.) ~ssrc sock =
  let open Lwt.Syntax in
  let buf = Bigstringaf.create 74 in
  let timeout () =
    Lwt_unix.sleep timeout |> Lwt.map (fun _ -> Error `Timeout)
  in
  let send_ip ~f ~ssrc sock =
    Packet.Ip.write ~ssrc f;
    let b = Faraday.serialize_to_string f |> Bytes.unsafe_of_string in
    let len = Bytes.length b in
    Lwt_unix.send sock b 0 len [] >|= fun n -> assert (n = len)
  in
  let recv_ip ~buf sock =
    let open Lwt.Syntax in
    let b = Bigstringaf.to_string buf |> Bytes.unsafe_of_string in
    let* _ = Lwt_unix.read sock b 0 74 in
    Bigstringaf.blit_from_bytes b ~src_off:0 buf ~dst_off:0 ~len:74;
    let pkt = Packet.Ip.of_bigstring buf in
    match pkt with Ok pkt -> Lwt.return pkt | Error msg -> failwith msg
  in
  let rec retry' n =
    let f = Faraday.of_bigstring buf in
    let* () = send_ip ~f ~ssrc sock in
    Faraday.close f;
    L.debug (fun m -> m "sent ip discovery request packet");
    let* o = Lwt.pick [ timeout (); recv_ip ~buf sock |> Lwt_result.ok ] in
    match o with
    | Ok (_, ip, port) -> Lwt_result.return (ip, port)
    | Error `Timeout when n > 0 ->
        L.warn (fun m ->
            m
              "timed out (%dx) waiting for ip discovery response packet, \
               retrying..."
              (retries - n + 1));
        retry' (n - 1)
    | Error `Timeout ->
        Lwt_result.fail "timed out waiting for ip discovery response"
  in
  retry' retries

type t = {
  sock : Lwt_unix.file_descr;
  local_addr : Unix.inet_addr * int;
  discord_addr : Unix.inet_addr * int;
  ssrc : int;
  f : Faraday.t;
  flush_packet : unit -> unit Lwt.t;
  send_tx : [ `Flush | `Keep_alive ] Lwt_pipe.Writer.t;
}

type encryption_mode = Normal | Suffix | Lite of uint32 ref

let encryption_mode_of_string = function
  | "xsalsa20_poly1305" -> Normal
  | "xsalsa20_poly1305_suffix" -> Suffix
  | "xsalsa20_poly1305_lite" ->
      Lite (ref (Random.int64 Int64.(2L ** 32L) |> Uint32.of_int64))
  | m -> raise @@ Invalid_argument ("unsupported encryption mode " ^ m)

type crypt = { mode : encryption_mode; secret : bytes }

let encrypt ~nonce ~secret data =
  let nonce = Sodium.Secret_box.Bigbytes.to_nonce nonce in
  let secret = Sodium.Secret_box.Bytes.to_key secret in
  Sodium.Secret_box.Bigbytes.secret_box secret data nonce

let send_voice_packet ~crypt ~ts ~seq ~audio t =
  let f = t.f in
  Packet.RTP.write_header ~seq ~ts ~ssrc:t.ssrc f;
  let encrypt nonce = encrypt audio ~nonce ~secret:crypt.secret in
  let () =
    match crypt.mode with
    | Normal ->
        let nonce = Faraday.create 24 in
        Packet.RTP.write_header ~seq ~ts ~ssrc:t.ssrc nonce;
        let padding =
          String.make
            (Sodium.Secret_box.nonce_size - Faraday.pending_bytes nonce)
            '\x00'
        in
        Faraday.write_string nonce padding;
        nonce |> Faraday.serialize_to_bigstring |> encrypt
        |> Faraday.schedule_bigstring f
    | Suffix ->
        let nonce = Sodium.Secret_box.(random_nonce () |> Bigbytes.of_nonce) in
        Faraday.schedule_bigstring f (encrypt nonce);
        Faraday.schedule_bigstring f nonce
    | Lite n ->
        let nonce = Bigstringaf.create Sodium.Secret_box.nonce_size in
        Bigstringaf.unsafe_set_int32_be nonce 0 (Uint32.to_int32 !n);
        let _padding =
          Seq.(
            4 --^ Bigstringaf.length nonce
            |> iter (fun i -> nonce.{i} <- '\x00'))
        in
        let next =
          if Uint32.(compare !n max_int) = 0 then Uint32.zero
          else Uint32.(!n + one)
        in
        n := next;
        Faraday.schedule_bigstring f (encrypt nonce);
        Faraday.schedule_bigstring f nonce
  in
  t.flush_packet ()

let create ~ssrc (ip, port) =
  let open Lwt.Syntax in
  let sock =
    Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
  in
  let* () = Lwt_unix.bind sock Unix.(ADDR_INET (inet_addr_any, 0)) in
  let addr = Unix.inet_addr_of_string ip in
  let* () = Lwt_unix.connect sock Unix.(ADDR_INET (addr, port)) in
  let discord_addr = (addr, port) in
  L.info (fun m ->
      m "discovering external ip using discord's voice server: %s:%d" ip port);
  let* ip_d, port_d =
    ip_discovery ~ssrc sock
    |> Lwt.map (function Ok d -> d | Error msg -> failwith msg)
  in
  L.info (fun m -> m "discovered ip and port: %s:%d" ip_d port_d);
  let local_addr = Unix.(inet_addr_of_string ip_d, port_d) in
  let f = Faraday.create (1024 * 4) in
  let writev = Faraday_lwt_unix.writev_of_fd sock in
  let send_tx = Lwt_pipe.create () in
  let t =
    {
      f;
      sock;
      local_addr;
      discord_addr;
      ssrc;
      flush_packet = (fun () -> Lwt_pipe.write send_tx `Flush >|= ignore);
      send_tx;
    }
  in
  let yield =
    let keep_alive =
      Lwt_stream.from (fun () ->
          Lwt_unix.sleep Packet.Keep_alive.interval >|= fun () ->
          Some `Keep_alive)
      |> Lwt_pipe.of_stream
    in
    Lwt_pipe.connect ~ownership:`OutOwnsIn keep_alive send_tx;
    fun f ->
      Lwt_pipe.read send_tx >|= function
      | Some `Keep_alive ->
          L.debug (fun m ->
              m "sending UDP keepalive packet"
                ~fields:
                  Relog.Field.
                    [
                      int "ssrc" ssrc;
                      float "interval" Packet.Keep_alive.interval;
                    ]);
          Packet.Keep_alive.write ~ssrc f
      | Some `Flush -> ()
      | None ->
          L.debug (fun m -> m "closing udp socket");
          Faraday.close f
  in
  let write_worker = Faraday_lwt_unix.serialize f ~yield ~writev in
  let read_worker =
    let open Lwt.Infix in
    let b = Bytes.create (1024 * 4) in
    let b_len = Bytes.length b in
    let rec f () =
      if Lwt_pipe.is_closed send_tx then Lwt.return_unit
      else
        let r = Lwt_unix.read sock b 0 b_len >|= fun n -> `read n in
        let t = Lwt_unix.sleep (1. /. 5.) >|= fun () -> `timeout in
        Lwt.pick [ r; t ] >>= function
        | `read _n ->
            (* L.trace (fun m ->
                m "got %d bytes from UDP (ssrc=%d) (rtcp? keepalive ack?)" n
                  ssrc); *)
            f ()
        | `timeout -> f ()
    in
    f ()
  in
  Lwt_pipe.keep send_tx write_worker;
  Lwt_pipe.keep send_tx read_worker;
  let+ () = Lwt_pipe.write_exn send_tx `Keep_alive in
  t

let destroy { sock; send_tx; _ } =
  match Lwt_unix.state sock with
  | Lwt_unix.Opened ->
      L.debug (fun m -> m "closing udp serializer");
      Lwt_pipe.close_nonblock send_tx
  | Closed | Aborted _ -> ()

let local_addr { local_addr; _ } = local_addr

let server_addr { discord_addr; _ } = discord_addr
