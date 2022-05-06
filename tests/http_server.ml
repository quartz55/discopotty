open Eio.Std
module Httpaf_server = Httpaf_eio.Server (Gluten_eio.Server)

let log_connection_error ex =
  traceln "Uncaught exception handling client: %a" Fmt.exn ex

let error_handler _addr : Httpaf.Server_connection.error_handler =
  let open Httpaf in
  fun ?request:_ error start_response ->
    traceln "err";
    let response_body = start_response Headers.empty in
    (match error with
    | `Exn exn ->
        Body.Writer.write_string response_body (Printexc.to_string exn);
        Body.Writer.write_string response_body "\n"
    | (`Bad_request | `Bad_gateway | `Internal_server_error) as http ->
        Body.Writer.write_string response_body
          (Status.default_reason_phrase http));
    Body.Writer.close response_body

let request_handler addr { Gluten.reqd; _ } =
  let open Httpaf in
  traceln "got req from %a" Eio.Net.Sockaddr.pp addr;
  match Reqd.request reqd with
  | { Request.meth = `POST; headers; _ } ->
      let response =
        let content_type =
          match Headers.get headers "content-type" with
          | None -> "application/octet-stream"
          | Some x -> x
        in
        Response.create
          ~headers:
            (Headers.of_list
               [ ("content-type", content_type); ("connection", "close") ])
          `OK
      in
      let request_body = Reqd.request_body reqd in
      let response_body =
        Reqd.respond_with_streaming ~flush_headers_immediately:true reqd
          response
      in
      let p, u = Promise.create () in
      let rec on_read buffer ~off ~len =
        Body.Writer.write_bigstring response_body buffer ~off ~len;
        Body.Reader.schedule_read request_body ~on_eof ~on_read
      and on_eof () =
        Body.Writer.close response_body;
        Promise.resolve u ()
      in
      Body.Reader.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read;
      Promise.await p
  | _ ->
      let headers = Headers.of_list [ ("connection", "close") ] in
      Reqd.respond_with_string reqd
        (Response.create ~headers `Method_not_allowed)
        ""

let main ~net =
  let port = 28080 in
  let listen_address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let handler =
    Httpaf_server.create_connection_handler ~request_handler ~error_handler
  in
  Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:5 net listen_address
  in
  traceln "listening on port %d" port;
  while true do
    Eio.Net.accept_sub socket ~sw ~on_error:log_connection_error
      (fun ~sw client_sock client_addr ->
        traceln "got connection request from %a" Eio.Net.Sockaddr.pp client_addr;
        Fun.protect
          (fun () -> handler ~sw client_addr client_sock)
          ~finally:(fun () -> Eio.Flow.close client_sock))
  done

let () =
  Utils.setup_logging ~level:Relog.Level.Trace ();
  Eio_luv.run @@ fun env -> main ~net:(Eio.Stdenv.net env)
