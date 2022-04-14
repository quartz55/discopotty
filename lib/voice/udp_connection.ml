open! Disco_core.Globals
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

let ip_discovery ?(retries = 3) ?(timeout = 1.) ~ssrc ~addr sock =
  let buf = Bigstringaf.create 74 in
  let timeout () =
    Eio_unix.sleep timeout;
    Error `Timeout
  in
  let send_ip ~f ~ssrc sock =
    Packet.Ip.write ~ssrc f;
    let b = Faraday.serialize_to_bigstring f |> Cstruct.of_bigarray in
    Eio.Net.send sock addr b
  in
  let recv_ip ~buf sock () =
    let b = Cstruct.of_bigarray ~len:74 buf in
    let _, n = Eio.Net.recv sock b in
    assert (n = 74);
    let pkt = Packet.Ip.of_bigstring buf in
    match pkt with Ok _ as r -> r | Error msg -> failwith msg
  in
  let rec retry' n =
    let f = Faraday.of_bigstring buf in
    send_ip ~f ~ssrc sock;
    Faraday.close f;
    L.debug (fun m -> m "sent ip discovery request packet");
    let o = Fiber.first timeout (recv_ip ~buf sock) in
    match o with
    | Ok (_, ip, port) -> Ok (ip, port)
    | Error `Timeout when n > 0 ->
        L.warn (fun m ->
            m
              "timed out (%dx) waiting for ip discovery response packet, \
               retrying..."
              (retries - n + 1));
        retry' (n - 1)
    | Error `Timeout -> Error "timed out waiting for ip discovery response"
  in
  retry' retries

type t = {
  sw : Switch.t;
  sock : Eio.Net.datagram_socket;
  local_addr : Eio.Net.Sockaddr.datagram;
  discord_addr : Eio.Net.Sockaddr.datagram;
  ssrc : int;
  send : (Faraday.t -> unit) -> unit;
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
  t.send @@ fun f ->
  Packet.RTP.write_header ~seq ~ts ~ssrc:t.ssrc f;
  let encrypt nonce = encrypt audio ~nonce ~secret:crypt.secret in
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
          4 --^ Bigstringaf.length nonce |> iter (fun i -> nonce.{i} <- '\x00'))
      in
      let next =
        if Uint32.(compare !n max_int) = 0 then Uint32.zero
        else Uint32.(!n + one)
      in
      n := next;
      Faraday.schedule_bigstring f (encrypt nonce);
      Faraday.schedule_bigstring f nonce

let create ~sw ~net ~ssrc (ip, port) =
  let p, u = Promise.create () in
  let run () : no_return =
    Switch.run @@ fun sw ->
    let discord_addr = `Udp (Eio.Net.Ipaddr.of_raw ip, port) in
    let sock = Eio.Net.datagram_socket ~sw net discord_addr in
    L.info (fun m ->
        m "discovering external ip using discord's voice server: %s:%d" ip port);
    let ip_d, port_d =
      ip_discovery ~ssrc ~addr:discord_addr sock |> Result.get_exn
    in
    L.info (fun m -> m "discovered ip and port: %s:%d" ip_d port_d);
    let local_addr = `Udp (Eio.Net.Ipaddr.of_raw ip_d, port_d) in
    let sync = Eio.Stream.create 0 in
    let send = Eio.Stream.add sync in
    let t = { sw; sock; local_addr; discord_addr; ssrc; send } in
    let write_worker () =
      let f = Faraday.create (1024 * 4) in
      let shutdown () =
        if not @@ Faraday.is_closed f then (
          Faraday.close f;
          Faraday.drain f |> ignore)
      in
      let writev = function
        | [] -> assert false
        | [ Faraday.{ buffer; off; len } ] ->
            let buf = Cstruct.of_bigarray ~off ~len buffer in
            Eio.Net.send sock discord_addr buf;
            len
        | vecs ->
            let len =
              List.fold_left (fun n Faraday.{ len; _ } -> n + len) 0 vecs
            in
            let buf = Cstruct.create_unsafe len in
            List.fold_left (fun dstoff Faraday.{ buffer; len; off } ->
                Cstruct.blit buffer off buf dstoff len;
                dstoff + len)
            |> ignore;
            Eio.Net.send sock discord_addr buf;
            len
      in
      let rec loop () =
        match Faraday.operation f with
        | `Writev iovecs ->
            let n = writev iovecs in
            Faraday.shift f n;
            loop ()
        | `Yield ->
            let fn = Eio.Stream.take sync in
            fn f;
            loop ()
        | `Close -> ()
      in
      Switch.on_release sw shutdown;
      Fun.protect ~finally:shutdown loop
    in
    let read_worker () =
      let buf = Cstruct.create (1024 * 4) in
      while true do
        let _addr, n = Eio.Net.recv sock buf in
        L.trace (fun m ->
            m "got %d bytes from UDP (ssrc=%d) (rtcp? keepalive ack?)" n ssrc)
      done
    in
    let keep_alive () =
      while true do
        send (fun f ->
            L.debug (fun m ->
                m "sending UDP keepalive packet"
                  ~fields:
                    Relog.Field.
                      [
                        int "ssrc" ssrc;
                        float "interval" Packet.Keep_alive.interval;
                      ]);
            Packet.Keep_alive.write ~ssrc f);
        Eio_unix.sleep Packet.Keep_alive.interval
      done
    in
    Promise.resolve u t;
    Fiber.any [ write_worker; read_worker; keep_alive ];
    raise Exit
  in
  Fiber.fork ~sw (fun () -> match run () with _ -> . | exception Exit -> ());
  Promise.await p

let destroy { sw; _ } = Switch.fail sw Exit
let local_addr { local_addr; _ } = local_addr
let server_addr { discord_addr; _ } = discord_addr
