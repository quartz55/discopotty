open! Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field
module Ws_client = Websocketaf_eio.Client (Gluten_eio.Client)
module Ws_payload = Websocketaf.Payload
open Eio.Std

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

let max_payload_len = 4096

let connect_tls ~sw host flow =
  L.trace (fun m -> m "configuring TLS client");
  let null ?ip:_ ~host:_ _certs = Ok None in
  let host = Domain_name.host_exn @@ Domain_name.of_string_exn host in
  let cfg =
    Tls.Config.client ~authenticator:null ~version:(`TLS_1_2, `TLS_1_3) ()
  in
  L.trace (fun m -> m "performing TLS handshake for socket");
  let out = Tls_eio.client_of_flow ~sw cfg ~host flow in
  (out :> eio_socket)

let open_socket ~sw ~net ?(tls = false) ?(port = if tls then 443 else 80) host =
  let rec inner = function
    | addr :: xs -> ( try Eio.Net.connect ~sw net addr with _ -> inner xs)
    | [] ->
        raise
        @@ Invalid_argument
             (Format.sprintf "couldn't connect socket to %s:%d" host port)
  in
  let addrs =
    Unix.getaddrinfo host (Int.to_string port) [ Unix.(AI_FAMILY PF_INET) ]
    |> List.map (fun addr ->
           match addr.Unix.ai_addr with
           | Unix.ADDR_INET (inetaddr, port) ->
               `Tcp (Eio_unix.Ipaddr.of_unix inetaddr, port)
           | ADDR_UNIX name -> `Unix name)
  in
  L.trace (fun m -> m "connecting socket");
  let flow = inner addrs in
  if tls then connect_tls ~sw host flow else flow

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
  let tls =
    match Uri.scheme uri with
    | Some "ws" | Some "http" -> false
    | Some "wss" | Some "https" | None -> true
    | Some scheme -> failwith ("unsupported websocket scheme: " ^ scheme)
  in
  let host = Uri.host uri |> Option.get_exn_or "empty host" in
  let port = Uri.port uri |> Option.get_or ~default:(if tls then 443 else 80) in
  let resource = Uri.path_and_query uri in
  (host, port, tls, resource)

type enc = [ `json ]

module type Connection = sig
  type conn_info = {
    addr : string * int;
    socket : eio_socket;
    enc : [ `json ];
    zlib : bool;
  }

  type send
  type recv

  type t
  and frame = Close of Close_code.t | Payload of recv

  val info : t -> conn_info
  val is_closed : t -> bool
  val read : t -> frame option
  val send : t -> send -> unit
  val close : ?code:Close_code.t -> t -> unit

  val create :
    sw:Switch.t -> net:Eio.Net.t -> ?zlib:bool -> ?enc:enc -> Uri.t -> t
end

exception Handshake_failure of string * Httpaf.Response.t
exception Http_failure of string

module Make : functor (P : Payload.Intf) ->
  Connection with type send := P.send and type recv := P.recv =
functor
  (P : Payload.Intf)
  ->
  struct
    type conn_info = {
      addr : string * int;
      socket : eio_socket;
      enc : [ `json ];
      zlib : bool;
    }

    type send = P.send
    type recv = P.recv

    type t = {
      info : conn_info;
      wsd : Websocketaf.Wsd.t;
      conn : Ws_client.t;
      stream : frame Eio.Stream.t;
    }

    and frame = Close of Close_code.t | Payload of recv

    let info { info; _ } = info
    let is_closed { wsd; _ } = Websocketaf.Wsd.is_closed wsd

    let read t =
      match is_closed t with
      | true -> Eio.Stream.take_nonblocking t.stream
      | false -> Some (Eio.Stream.take t.stream)

    let send t (pl : send) =
      L.debug (fun m ->
          let pp = P.to_json pl |> Yojson.Safe.pretty_to_string in
          m "sending payload:@.%s" pp);
      let out = pl |> P.to_bytes in
      let len = Bytes.length out in
      let rec send' n =
        match len - n with
        | len when len > max_payload_len ->
            (* TODO this hasn't been actually tested *)
            Websocketaf.Wsd.send_bytes t.wsd ~kind:`Continuation out ~off:n
              ~len:max_payload_len;
            send' (n + max_payload_len)
        | len -> Websocketaf.Wsd.send_bytes t.wsd ~kind:`Text out ~off:n ~len
      in
      send' 0

    let close ?code t =
      if not @@ is_closed t then (
        Websocketaf.Wsd.close ?code t.wsd;
        Fiber.yield ();
        Ws_client.shutdown t.conn)

    let rec _connect ~sw ~net ~enc ~zlib ~tls host port resource =
      L.info (fun m -> m "opening socket (%s:%d) tls=%b" host port tls);
      let socket = open_socket ~sw ~net ~tls ~port host in
      let conn_info = { addr = (host, port); socket; enc; zlib } in

      let p, u = Promise.create () in
      let error_handler = function
        | `Handshake_failure (rsp, _)
          when Httpaf.(Status.is_redirection rsp.Response.status) ->
            let loc =
              Httpaf.Headers.get rsp.headers "Location"
              |> Option.get_exn_or
                   "missing location header in redirect response"
            in
            L.warn (fun m ->
                m "got redirection to '%s' during websocket handshake" loc
                  ~fields:Relog.Field.[ str "location" loc ]);
            Promise.resolve_ok u (`Redir loc)
        | `Handshake_failure (rsp, _body) ->
            L.err (fun m ->
                m "error during websocket handshake:@.%a" Httpaf.Response.pp_hum
                  rsp);
            let emsg =
              Format.sprintf "failed to handshake websocket: %s" rsp.reason
            in
            Promise.resolve_error u @@ Handshake_failure (emsg, rsp)
        | `Malformed_response msg -> Promise.resolve_error u @@ Http_failure msg
        | `Invalid_response_body_length _res ->
            Promise.resolve_error u
            @@ Http_failure "invalid response body length"
        | `Exn exn -> Promise.resolve_error u @@ exn
      in
      let websocket_handler wsd =
        (* TODO @quartz55: does this effectively back-pressure
           or will it just be catastrophic?
           maybe an unbounded stream would make more sense
           given ws/af is push-based *)
        let stream = Eio.Stream.create max_int in
        Promise.resolve_ok u (`Conn (conn_info, wsd, stream));
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
        let frame ~opcode ~is_fin ~len payload =
          Fiber.fork ~sw @@ fun () ->
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
          | `Connection_close when len < 2 ->
              L.warn (fun m -> m "no close code in frame, defaulting to 1000");
              Eio.Stream.add stream (Close `Going_away);
              Websocketaf.Wsd.close wsd
          | `Connection_close ->
              let frame = read payload in
              let close_code =
                Bigstringaf.get_int16_be frame 0 |> Close_code.of_int_exn
              in
              L.warn (fun m ->
                  m "got close frame with code=%a" Close_code.pp close_code);
              Eio.Stream.add stream (Close close_code);
              Websocketaf.Wsd.close wsd
          | `Binary | `Text ->
              (* TODO zlib transport compression *)
              let frame = read payload in
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
              let pl =
                try Some (P.of_json pl_json)
                with
                | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exn, _)
                | exn
                ->
                  L.warn (fun m ->
                      m "couldn't parse payload, ignoring..."
                        ~fields:F.[ str "exn" (Printexc.to_string exn) ]);
                  None
              in
              Option.iter (fun pl -> Eio.Stream.add stream (Payload pl)) pl
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
          raise @@ Sys_error "websocket received EOF"
        in
        { Websocketaf.Client_connection.frame; eof }
      in
      L.info (fun m -> m "initiating websocket handshake");
      let nonce = gen_nonce 20 in
      let conn =
        Ws_client.connect ~sw socket ~nonce ~host ~port ~resource ~error_handler
          ~websocket_handler
      in
      match Promise.await_exn p with
      | `Conn (info, wsd, stream) -> { info; wsd; conn; stream }
      | `Redir loc ->
          let host, port, tls, resource = _uri_info (Uri.of_string loc) in
          _connect ~sw ~net ~enc ~zlib ~tls host port resource

    let create ~sw ~net ?(zlib = false) ?(enc = `json) uri =
      L.info (fun m -> m "connecting to gateway at '%a'" Uri.pp uri);
      let host, port, tls, resource = _uri_info uri in
      let t = _connect ~sw ~net ~enc ~zlib ~tls host port resource in
      Switch.on_release sw (fun () -> close t);
      t
  end
