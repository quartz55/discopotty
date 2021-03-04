open Globals

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

    let of_ic_lwt ic =
      let open Lwt.Syntax in
      let+ { len; _ }, res = Angstrom_lwt_unix.parse P.Ip.packet ic in
      match (len, res) with
      | 0, Ok pkt -> Ok pkt
      | o, Ok _ ->
          L.error (fun m ->
              m "%d unconsummed bytes when deserialising UDP packet?" o);
          assert false
      | _, Error msg -> Error msg

    let write ~ssrc f =
      Faraday.BE.write_uint16 f 0x1;
      Faraday.BE.write_uint16 f 70;
      Faraday.BE.write_uint32 f (Int32.of_int ssrc);
      Faraday.write_bytes f (Bytes.create 66)
  end
end

let _send_ip ?(f = Faraday.create 74) ~ssrc sock =
  Packet.Ip.write ~ssrc f;
  let b = Faraday.serialize_to_string f |> Bytes.unsafe_of_string in
  let len = Bytes.length b in
  Lwt_unix.send sock b 0 len [] |> Lwt.map (fun n -> assert (n = len))

let _recv_ip ?(buf = Bigstringaf.create 74) sock =
  let open Lwt.Syntax in
  let b = Bigstringaf.to_string buf |> Bytes.unsafe_of_string in
  let* _ = Lwt_unix.read sock b 0 74 in
  Bigstringaf.blit_from_bytes b ~src_off:0 buf ~dst_off:0 ~len:74;
  let pkt = Packet.Ip.of_bigstring buf in
  match pkt with Ok pkt -> Lwt.return pkt | Error msg -> failwith msg

let ip_discovery ?(retries = 3) ?(timeout = 1.) ~ssrc sock =
  let open Lwt.Syntax in
  let buf = Bigstringaf.create 74 in
  let timeout () =
    Lwt_unix.sleep timeout |> Lwt.map (fun _ -> Error `Timeout)
  in
  let rec retry' n =
    let f = Faraday.of_bigstring buf in
    let* () = _send_ip ~f ~ssrc sock in
    Faraday.close f;
    L.debug (fun m -> m "sent ip discovery request packet");
    let* o = Lwt.pick [ timeout (); _recv_ip ~buf sock |> Lwt_result.ok ] in
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
  f : Faraday.t;
  sock : Lwt_unix.file_descr;
  ext_addr : string * int;
  ssrc : int;
}

let create ~ssrc (ip, port) =
  let open Lwt.Syntax in
  let sock =
    Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      (Unix.getprotobyname "udp").Unix.p_proto
    |> Lwt_unix.of_unix_file_descr
  in
  let addr = Unix.inet_addr_of_string ip in
  let sockaddr = Unix.ADDR_INET (addr, port) in
  L.info (fun m -> m "connecting UDP socket to %s:%d" ip port);
  let* () = Lwt_unix.connect sock sockaddr in
  L.info (fun m -> m "discovering external ip...");
  let+ ip_d, port_d =
    ip_discovery ~ssrc sock
    |> Lwt.map (function Ok d -> d | Error msg -> failwith msg)
  in
  L.info (fun m -> m "discovered ip and port: %s:%d" ip_d port_d);
  let f = Faraday.create (1024 * 4) in
  { f; sock; ext_addr = (ip_d, port_d); ssrc }

let close { f; sock; _ } =
  Faraday.drain f |> ignore;
  Lwt_unix.close sock
