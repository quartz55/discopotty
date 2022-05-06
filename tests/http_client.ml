module Http = Disco_http
open Eio.Std

let test_get env =
  let net = Eio.Stdenv.net env in
  let uri = Uri.of_string "https://jsonplaceholder.typicode.com" in
  let c = Http.Client.create ~net ~uri () in
  Switch.run @@ fun sw ->
  let headers = [ ("Accept", "application/json") ] in
  let res, body = Http.Client.get ~sw c ~headers "/posts/1" in
  if Httpaf.Status.is_successful res.status then (
    let json = Http.Body.to_string body |> Yojson.Safe.from_string in
    traceln "ok: %a" Httpaf.Response.pp_hum res;
    traceln "%a" (Yojson.Safe.pretty_print ~std:false) json)
  else
    let body = Http.Body.to_string body in
    let p = try Yojson.Safe.(from_string body |> to_string) with _ -> body in
    traceln "error: %a@.%s" Httpaf.Response.pp_hum res p

let test_delayed_post env =
  let net = Eio.Stdenv.net env in
  let uri = Uri.of_string "https://jsonplaceholder.typicode.com" in
  let c = Http.Client.create ~net ~uri () in
  Switch.run @@ fun sw ->
  let headers =
    [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
  in
  let req, do_req =
    let data =
      `Assoc
        [
          ("title", `String "foo"); ("body", `String "bar"); ("userId", `Int 1);
        ]
      |> Yojson.Safe.to_string
    in
    let len = String.length data in
    let p, u = Promise.create () in
    let flow =
      object (_ : < Eio.Flow.source >)
        inherit Eio.Flow.source
        val mutable read = 0

        method read_into cbuf =
          if not @@ Promise.is_resolved p then Promise.await p;
          match (max 0 (len - read), Cstruct.length cbuf) with
          | 0, _ -> raise End_of_file
          | len, clen ->
              let to_write = min len clen in
              Cstruct.blit_from_string data read cbuf 0 to_write;
              read <- read + to_write;
              to_write
      end
    in
    ( Http.Body.of_flow ~length:(`Fixed Int64.(of_int len)) flow,
      Promise.resolve u )
  in
  let handler () =
    let res, body =
      Http.Client.call ~sw c ~meth:`POST ~headers ~body:req "/posts"
    in
    if Httpaf.Status.is_successful res.status then (
      let json = Http.Body.to_string body |> Yojson.Safe.from_string in
      traceln "ok: %a" Httpaf.Response.pp_hum res;
      traceln "%a" (Yojson.Safe.pretty_print ~std:false) json)
    else
      let body = Http.Body.to_string body in
      let p =
        try Yojson.Safe.(from_string body |> to_string) with _ -> body
      in
      traceln "error: %a@.%s" Httpaf.Response.pp_hum res p
  in
  Fiber.both
    (fun () ->
      Eio_unix.sleep 1.;
      traceln "actually sending body";
      do_req ())
    handler

let () =
  Utils.setup_logging ~level:Relog.Level.Trace ();
  Eio_luv.run test_delayed_post
