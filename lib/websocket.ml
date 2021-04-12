open Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module F = Relog.Field
module Ws_client = Websocketaf_lwt.Client (Gluten_lwt_unix.Client)
module Wss_client = Websocketaf_lwt.Client (Gluten_lwt_unix.Client.SSL)
module Ws_payload = Websocketaf.Payload

module Close_code = struct
  include Websocketaf.Websocket.Close_code

  let is_standard = function #standard -> true | _ -> false

  let is_std = is_standard

  let pp =
    let pp' fmt = function
      | `Normal_closure -> Format.fprintf fmt "ok"
      | `Going_away -> Format.fprintf fmt "going away"
      | `Protocol_error -> Format.fprintf fmt "protocol error"
      | `Unsupported_data -> Format.fprintf fmt "unsupported data"
      | `No_status_rcvd -> Format.fprintf fmt "no status received"
      | `Abnormal_closure -> Format.fprintf fmt "abnormal closure"
      | `Invalid_frame_payload_data ->
          Format.fprintf fmt "invalid frame payload data"
      | `Policy_violation -> Format.fprintf fmt "policy violation"
      | `Message_too_big -> Format.fprintf fmt "message too big"
      | `Mandatory_ext -> Format.fprintf fmt "mandatory exit"
      | `Internal_server_error -> Format.fprintf fmt "internal server error"
      | `TLS_handshake -> Format.fprintf fmt "TLS handshake"
      | `Other _ -> Format.fprintf fmt "_"
    in
    fun fmt t -> Format.fprintf fmt "(%d %a)" (to_int t) pp' t
end

let () = Ssl.init ()

let max_payload_len = 4096

let connect_ssl host fd =
  L.info (fun m -> m "initializing SSL context");
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
  let ssl_sock = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
  Ssl.set_client_SNI_hostname ssl_sock host;
  Ssl.set_hostflags ssl_sock [ No_partial_wildcards ];
  Ssl.set_host ssl_sock host;
  L.info (fun m -> m "performing SSL handshake for socket");
  let open Lwt.Syntax in
  let+ socket_or_error =
    Lwt.catch
      (fun () -> Lwt_result.ok (Lwt_ssl.ssl_perform_handshake s))
      (function
        | Ssl.Connection_error _ssl_error ->
            let msg = Ssl.get_error_string () in
            L.err (fun m -> m "error performing SSL handshake: %s" msg);
            Lwt_result.fail (`Msg msg)
        | _ -> assert false)
  in
  match socket_or_error with
  | Ok ssl_socket ->
      let _ssl_version = Ssl.version ssl_sock in
      let ssl_cipher = Ssl.get_cipher ssl_sock in
      L.info (fun m ->
          m "SSL connection using TLS1.2 / %s" (Ssl.get_cipher_name ssl_cipher));
      Ok ssl_socket
  | Error e ->
      let verify_result = Ssl.get_verify_result ssl_sock in
      if verify_result <> 0 then
        L.err (fun m -> m "verify_result=%d" verify_result);
      Error e

let open_socket ?(ssl = false) ?(port = if ssl then 443 else 80) host =
  let open Lwt_result.Syntax in
  let* addresses =
    Lwt_unix.getaddrinfo host (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ]
    |> Lwt_result.catch
    |> Lwt_result.map_err (fun e -> `Exn e)
  in

  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  L.info (fun m -> m "connecting socket");
  let* () =
    let rec inner = function
      | addr :: xs ->
          Lwt.try_bind
            (fun () -> Lwt_unix.connect fd addr.Unix.ai_addr)
            (fun () -> Lwt_result.return ())
            (fun _exn -> inner xs)
      | [] ->
          Lwt_result.fail
            (`Msg
              (Format.asprintf "couldn't connect socket to %s:%d" host port))
    in
    inner addresses
  in
  let+ socket =
    if ssl then connect_ssl host fd else Lwt_ssl.plain fd |> Lwt_result.return
  in
  socket

let gen_nonce =
  let alphanum =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  in
  let len = String.length alphanum in
  let rnd = Random.State.make_self_init () in
  fun ?(rnd = rnd) n ->
    let str = Bytes.create n in
    for i = 0 to pred n do
      Bytes.set str i alphanum.[Random.int len rnd]
    done;
    str |> Bytes.unsafe_to_string

let _uri_info uri =
  let ssl =
    match Uri.scheme uri with
    | Some "ws" | Some "http" -> false
    | Some "wss" | Some "https" | None -> true
    | Some scheme -> failwith ("unsupported websocket scheme: " ^ scheme)
  in
  let host = Uri.host uri |> Option.get_exn in
  let port = Uri.port uri |> Option.get_or ~default:(if ssl then 443 else 80) in
  let resource = Uri.path_and_query uri in
  (host, port, ssl, resource)

type enc = [ `json ]

module type Connection = sig
  type conn_info = {
    addr : string * int;
    socket : Lwt_ssl.socket;
    enc : [ `json ];
    zlib : bool;
  }

  type send

  type recv

  type t

  and frame = Close of Close_code.t | Payload of recv

  val info : t -> conn_info

  val stream : t -> frame Lwt_stream.t

  val send : t -> send -> unit

  val close : ?code:Close_code.t -> t -> unit

  val create : ?zlib:bool -> ?enc:enc -> Uri.t -> (t, Error.t) Lwt_result.t
end

module Make : functor (P : Payload.Intf) ->
  Connection with type send := P.send and type recv := P.recv =
functor
  (P : Payload.Intf)
  ->
  struct
    type conn_info = {
      addr : string * int;
      socket : Lwt_ssl.socket;
      enc : [ `json ];
      zlib : bool;
    }

    type send = P.send

    type recv = P.recv

    type t = {
      info : conn_info;
      wsd : Websocketaf.Wsd.t;
      stream : frame Lwt_stream.t;
    }

    and frame = Close of Close_code.t | Payload of recv

    let info { info; _ } = info

    let stream { stream; _ } = stream

    let send t (pl : send) =
      L.debug (fun m ->
          let pp = P.to_json pl |> Yojson.Safe.pretty_to_string in
          m "sending payload:@.%s" pp);
      let out = pl |> P.to_bytes in
      let len = Bytes.length out in
      let rec send' n =
        match len - n with
        | len when len > max_payload_len ->
            Websocketaf.Wsd.send_bytes t.wsd ~kind:`Continuation out ~off:n
              ~len:max_payload_len;
            send' (n + max_payload_len)
        | len -> Websocketaf.Wsd.send_bytes t.wsd ~kind:`Text out ~off:n ~len
      in
      send' 0

    let close ?code t = Websocketaf.Wsd.close ?code t.wsd

    let rec _connect ~enc ~zlib ~ssl host port resource =
      let open Lwt_result.Syntax in
      L.info (fun m -> m "opening socket (%s:%d) ssl=%b" host port ssl);
      let* socket = open_socket ~ssl ~port host in
      let conn_info = { addr = (host, port); socket; enc; zlib } in

      L.info (fun m -> m "initiating websocket handshake");
      let nonce = gen_nonce 20 in

      let p, u = Lwt.wait () in
      let error_handler = function
        | `Handshake_failure (rsp, _)
          when Httpaf.(Status.is_redirection rsp.Response.status) ->
            let loc =
              Httpaf.Headers.get rsp.headers "Location" |> Option.get_exn
            in
            L.warn (fun m ->
                m "got redirection to '%s' during websocket handshake" loc
                  ~fields:Relog.Field.[ str "location" loc ]);
            Lwt.wakeup_later u (Error (`Redir loc))
        | `Handshake_failure (rsp, _body) ->
            L.err (fun m ->
                m "error during websocket handshake:@.%a" Httpaf.Response.pp_hum
                  rsp);
            let reason = rsp.reason in
            Lwt.wakeup_later u
              (Error (`Msg ("failed to handshake websocket: " ^ reason)))
        | _ -> assert false
      in
      let websocket_handler wsd =
        let stream, push = Lwt_stream.create () in
        Lwt.wakeup_later u (Ok { info = conn_info; wsd; stream });
        let do_read ~on_done payload =
          let buf = Faraday.create 1024 in
          let on_eof () =
            L.trace (fun m -> m "done reading!");
            let out = Faraday.serialize_to_bigstring buf in
            on_done out
          in
          let rec on_read bs ~off ~len =
            L.trace (fun m -> m "on_read (off=%d) (len=%d)" off len);
            Faraday.schedule_bigstring buf bs ~off ~len;
            Ws_payload.schedule_read payload ~on_read ~on_eof
          in
          Ws_payload.schedule_read payload ~on_read ~on_eof
        in
        let frame ~opcode ~is_fin ~len payload =
          L.debug (fun m ->
              m "frame"
                ~fields:
                  F.
                    [
                      int "opcode" (Websocketaf.Websocket.Opcode.to_int opcode);
                      bool "is_fin" is_fin;
                      int "len" len;
                    ]);
          match opcode with
          | `Connection_close ->
              if len < 2 then failwith "no close code in frame";
              let on_done frame =
                let close_code =
                  Bigstringaf.get_int16_be frame 0 |> Close_code.of_int_exn
                in
                L.warn (fun m ->
                    m "got close frame with code=%a" Close_code.pp close_code);
                push (Some (Close close_code));
                push None
              in
              do_read payload ~on_done
          | `Binary | `Text ->
              (* TODO zlib transport compression *)
              let on_done frame =
                let pl_json =
                  try Bigstringaf.to_string frame |> Yojson.Safe.from_string
                  with exn ->
                    L.err (fun m ->
                        m "invalid json payload"
                          ~fields:F.[ str "exn" (Printexc.to_string exn) ]);
                    raise exn
                in
                L.debug (fun m ->
                    m "got websocket payload:@.%s"
                      (pl_json |> Yojson.Safe.pretty_to_string));
                match P.of_json pl_json with
                | (exception
                    Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exn, _))
                | (exception exn) ->
                    L.warn (fun m ->
                        m "couldn't parse payload, ignoring..."
                          ~fields:F.[ str "exn" (Printexc.to_string exn) ])
                | pl -> push (Some (Payload pl))
              in
              do_read payload ~on_done
          | `Continuation ->
              L.err (fun m -> m "continuation frame unsupported");
              exit (-1)
          | `Ping -> L.warn (fun m -> m "got PING frame, ignoring... (?)")
          | `Pong -> L.warn (fun m -> m "got PONG frame, ignoring... (?)")
          | `Other _ ->
              L.warn (fun m -> m "got non standard code frame, ignoring...")
        in
        let eof () =
          L.err (fun m -> m "!!!FATAL!!! websocket received EOF");
          exit (-1)
        in
        { Websocketaf.Client_connection.frame; eof }
      in
      let do_handshake =
        if ssl then
          Wss_client.connect socket ~nonce ~host ~port ~resource ~error_handler
            ~websocket_handler
          >|= ignore
        else
          Ws_client.connect (Lwt_ssl.get_fd socket) ~nonce ~host ~port ~resource
            ~error_handler ~websocket_handler
          >|= ignore
      in
      let* _ = do_handshake |> Error.catch_lwt in
      p >>= function
      | Error (`Redir loc) ->
          let host, port, ssl, resource = _uri_info (Uri.of_string loc) in
          _connect ~enc ~zlib ~ssl host port resource
      | Error (`Msg _) as err -> Lwt.return err
      | Ok t -> Lwt.return (Ok t)

    let create ?(zlib = false) ?(enc = `json) uri =
      L.info (fun m -> m "connecting to gateway at '%a'" Uri.pp uri);
      let host, port, ssl, resource = _uri_info uri in
      _connect ~enc ~zlib ~ssl host port resource
  end
