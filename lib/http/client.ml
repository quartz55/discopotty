open Containers
open Eio.Std
module L = (val Relog.logger ~namespace:"Disco_http.Client" ())
module F = Relog.Field
module Http_client = Httpaf_eio.Client (Gluten_eio.Client)

module Headers = struct
  include Httpaf.Headers

  let canonicalize ~body_length ~host t =
    let h = add_unless_exists t "Host" host in
    match body_length with
    | `Fixed n -> add_unless_exists h "content-length" (Int64.to_string n)
    | `Chunked -> add_unless_exists h "transfer-encoding" "chunked"
    | `Close_delimited -> add_unless_exists h "connection" "close"
    | `Error _ | `Unknown -> h
end

module Status = struct
  include Httpaf.Status

  let is_permanent_redirection = function
    | `Moved_permanently
    (* https://tools.ietf.org/html/rfc7538#section-3 *)
    | `Code 308 ->
        true
    | _ -> false
end

exception Http_error of string
exception Invalid_scheme of string

let base_url = "https://discord.com"
let base_uri = Uri.of_string base_url

type eio_socket = < Eio.Flow.two_way ; Eio.Flow.close >
type t = { net : Eio.Net.t; mutable uri : Uri.t; mutable base_path : string }

type conn = { http : Http_client.t; socket : eio_socket; info : conn_info }

and conn_info = {
  host : string;
  port : int;
  tls : bool;
  addrs : Gluten_eio.Eio_io.addr list;
}

let conn_info uri =
  let tls =
    match Uri.scheme uri with
    | Some "http" -> false
    | Some "https" | None -> true
    | Some scheme -> raise @@ Invalid_scheme scheme
  in
  let host = Uri.host uri |> Option.get_exn_or "empty host" in
  let port = Uri.port uri |> Option.get_or ~default:(if tls then 443 else 80) in
  let addrs =
    Unix.getaddrinfo host (Int.to_string port)
      Unix.[ AI_CANONNAME; AI_PROTOCOL 6; AI_FAMILY PF_INET ]
    |> List.map (fun addr ->
           match addr.Unix.ai_addr with
           | Unix.ADDR_INET (inetaddr, port) ->
               `Tcp (Eio_unix.Ipaddr.of_unix inetaddr, port)
           | ADDR_UNIX name -> `Unix name)
  in
  { host; port; tls; addrs }

let connect_tls ~(l : Relog.logger) ~sw host flow =
  let module L = (val l) in
  L.trace (fun m -> m "configuring TLS client");
  let null ?ip:_ ~host:_ _certs = Ok None in
  let host =
    Domain_name.of_string_exn host |> Domain_name.host |> Option.of_result
  in
  let cfg = Tls.Config.client ~authenticator:null () in
  L.trace (fun m -> m "performing TLS handshake for socket");
  let out = Tls_eio.client_of_flow ~sw ?host cfg flow in
  (out :> eio_socket)

let open_socket ~(l : Relog.logger) ~sw ~net ?(tls = false)
    ?(port = if tls then 443 else 80) host =
  let module L = (val l) in
  let rec inner = function
    | addr :: xs -> (
        L.trace (fun m -> m "trying %a" Eio.Net.Sockaddr.pp addr);
        try Eio.Net.connect ~sw net addr with _ -> inner xs)
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
  if tls then connect_tls ~l ~sw host flow else flow

let create_conn ~(l : Relog.logger) ~sw t uri =
  let info = conn_info uri in
  let socket =
    open_socket ~l ~sw ~net:t.net ~tls:info.tls ~port:info.port info.host
  in
  let module L = (val l) in
  let fields =
    L.fields
    |> Relog.Fields.add F.(str "type" "http")
    |> Relog.Fields.to_seq |> List.of_seq
  in
  let module L = (val Relog.logger ~fields ()) in
  let http = Http_client.create_connection ~l:(module L) ~sw socket in
  { http; socket; info }

let drain body =
  let p, u = Promise.create () in
  let on_eof = Promise.resolve u in
  let rec on_read _bs ~off:_ ~len:_ =
    Httpaf.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  Httpaf.Body.Reader.schedule_read body ~on_read ~on_eof;
  Promise.await p;
  Httpaf.Body.Reader.close body

let rec handle ~(l : Relog.logger) ~sw t ?(uri = t.uri) ?(body = Body.empty) req
    =
  let module L = (val l) in
  let conn = create_conn ~l ~sw t uri in
  let p, u = Promise.create () in
  let error_handler = function
    | `Malformed_response msg -> Promise.resolve_error u @@ Http_error msg
    | `Invalid_response_body_length _res ->
        Promise.resolve_error u @@ Http_error "invalid response body length"
    | `Exn exn -> Promise.resolve_error u exn
  in
  let response_handler res res_body = Promise.resolve_ok u (res, res_body) in
  let body_length = Body.length body in
  let canonical_headers =
    Headers.canonicalize ~body_length ~host:conn.info.host
      req.Httpaf.Request.headers
  in
  let req_body =
    Http_client.request ~flush_headers_immediately:true ~error_handler
      ~response_handler conn.http
      { req with headers = canonical_headers }
  in
  Fiber.fork ~sw (fun () -> Body.write body req_body);
  L.trace (fun m -> m "req: %a" Httpaf.Request.pp_hum req);
  let res, res_body = Promise.await_exn p in
  match (res.status, Headers.get res.headers "location") with
  | status, Some loc_str when Status.is_redirection status ->
      let location = Uri.of_string loc_str in
      let new_uri =
        match Uri.host location with
        | Some _ -> location
        | None ->
            Uri.resolve
              (if conn.info.tls then "https" else "http")
              t.uri location
      in
      L.trace (fun m -> m "handling redirection to %a" Uri.pp new_uri);
      if Status.is_permanent_redirection status then (
        t.uri <- new_uri;
        L.warn @@ fun m -> m "moved permanently to: %a" Uri.pp t.uri);
      let target = Uri.path_and_query new_uri in
      let meth =
        match (req.meth, res.status) with
        | `POST, (`Found | `Moved_permanently) -> `GET
        | _ -> req.meth
      in
      let req = Httpaf.Request.{ req with meth; target } in
      drain res_body;
      Http_client.shutdown conn.http;
      handle ~l ~sw t ~uri:new_uri ~body req
  | _ ->
      let request_method =
        match req.meth with
        | #Httpaf.Method.standard as meth -> meth
        | `Other _ -> `GET
      in
      (* TODO: revisit whether this is necessary. *)
      (match request_method with
      | `HEAD -> Httpaf.Body.Reader.close res_body
      | _ -> ());
      let length =
        (Httpaf.Response.body_length ~request_method res :> Body.length)
      in
      let body =
        Body.of_httpaf
          ~on_close:(fun () -> Http_client.shutdown conn.http)
          ~length res_body
      in
      L.trace (fun m -> m "res: %a" Httpaf.Response.pp_hum res);
      Switch.on_release sw (fun () ->
          Body.drain body;
          Http_client.shutdown conn.http);
      (res, body)

let call ?sw t ~meth ?(headers = []) ?body target =
  let version = Httpaf.Version.{ major = 1; minor = 1 } in
  let headers = Httpaf.Headers.of_list headers in
  let req = Httpaf.Request.create ~version ~headers meth target in
  let module L =
  (val Relog.clone
         ~fields:
           F.
             [
               str "method" (Httpaf.Method.to_string meth); str "target" target;
             ]
         (module L))
  in
  let with_sw sw =
    let res, body = handle ~l:(module L) ~sw t ?body req in
    L.dbg (fun m ->
        m "%a %s -> %a" Httpaf.Method.pp_hum meth target Httpaf.Status.pp_hum
          res.status);
    (res, body)
  in
  match sw with
  | Some sw -> with_sw sw
  | None ->
      Switch.run @@ fun sw ->
      let r, b = with_sw sw in
      (r, Body.consume b)

let get ?sw t = call ?sw t ~meth:`GET ~body:Body.empty
let put ?sw t = call ?sw t ~meth:`PUT
let post ?sw t = call ?sw t ~meth:`POST
let delete ?sw t = call ?sw t ~meth:`DELETE
let del = delete

let create ~net ?(uri = base_uri) () =
  { net; uri; base_path = Uri.path_and_query uri }
