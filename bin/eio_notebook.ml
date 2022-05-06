module Ws_client = Websocketaf_eio.Client (Gluten_eio.Client)
module Ws_payload = Websocketaf.Payload
open Eio.Std

let websocket_handler ~sw ~stdin u wsd =
  let read_line =
    let r = Eio.Buf_read.of_flow ~max_size:4096 stdin in
    fun () -> Eio.Buf_read.line r
  in
  let rec input_loop wsd () =
    let line = read_line () in
    let payload = Bytes.of_string line in
    Websocketaf.Wsd.send_bytes wsd ~kind:`Text payload ~off:0
      ~len:(Bytes.length payload);
    if line = "exit" then Websocketaf.Wsd.close wsd else input_loop wsd ()
  in
  let read payload =
    let p, u = Promise.create () in
    let buf = Faraday.create 1024 in
    let on_eof () =
      let out = Faraday.serialize_to_bigstring buf in
      Promise.resolve u out
    in
    let rec on_read bs ~off ~len =
      Faraday.schedule_bigstring buf bs ~off ~len;
      Ws_payload.schedule_read payload ~on_read ~on_eof
    in
    Ws_payload.schedule_read payload ~on_read ~on_eof;
    Promise.await p
  in
  Fiber.fork ~sw (input_loop wsd);
  let frame ~opcode:_ ~is_fin:_ ~len:_ payload =
    Fiber.fork ~sw @@ fun () ->
    let b = read payload in
    Format.printf "%s@." (Bigstringaf.to_string b)
  in
  let eof () =
    Printf.eprintf "[EOF]\n%!";
    Promise.resolve u ()
  in
  { Websocketaf.Client_connection.frame; eof }

let error_handler = function
  | `Handshake_failure (rsp, _body) ->
      Format.eprintf "Handshake failure: %a\n%!" Httpaf.Response.pp_hum rsp
  | _ -> assert false

let () =
  let host = ref None in
  let port = ref 80 in

  Arg.parse
    [ ("-p", Set_int port, " Port number (80 by default)") ]
    (fun host_argument -> host := Some host_argument)
    "wscat.exe [-p N] HOST";

  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in

  Eio_unix.Ctf.with_tracing "wsaf.ctf" @@ fun () ->
  Eio_luv.run @@ fun env ->
  Switch.run @@ fun sw ->
  let addresses =
    Unix.getaddrinfo host (string_of_int !port) [ Unix.(AI_FAMILY PF_INET) ]
    |> List.map (fun addr ->
           match addr.Unix.ai_addr with
           | Unix.ADDR_INET (inetaddr, port) ->
               `Tcp (Eio_unix.Ipaddr.of_unix inetaddr, port)
           | ADDR_UNIX name -> `Unix name)
  in
  let stdin, net = Eio.Stdenv.(stdin env, net env) in
  let socket = Eio.Net.connect ~sw net (List.hd addresses) in
  let nonce = "0123456789ABCDEF" in
  let resource = "/" in
  let port = !port in
  let p, u = Promise.create () in
  let _conn =
    Ws_client.connect ~sw socket ~nonce ~host ~port ~resource ~error_handler
      ~websocket_handler:(websocket_handler ~sw ~stdin u)
  in
  Promise.await p
(* Ws_client.shutdown conn *)
