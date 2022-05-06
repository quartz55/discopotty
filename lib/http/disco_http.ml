open Containers
open Eio.Std
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field
module Client = Client
module Body = Body
module Token_bucket = Disco_utils.Lf_token_bucket

let user_agent = "discopotty (github.com/quartz55/discopotty-re, v0.0.1)"

(* TODO @quartz55 *)
module ApiError = struct
  type t = { code : int; (* errors : errors; *) message : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let code { code; _ } = code
  let message { message; _ } = message
  let pp fmt { code; message; _ } = Format.fprintf fmt "(%d: %s)" code message
end

type t = {
  token : string;
  version : Version.t;
  client : Client.t;
  global_tb : Token_bucket.t;
  err_tb : Token_bucket.t;
  errs : int Atomic.t;
}

let create ~net ?(version = Version.V9) token =
  let client = Client.create ~net () in
  (* 50 requests per second *)
  let rate = 1. /. 50. in
  let global_tb = Token_bucket.make ~capacity:50 rate in
  (* 10,000 per 10 minutes *)
  let rate = 1. /. (10_000. /. (10. *. 60.)) in
  let err_tb = Token_bucket.make ~capacity:10_000 rate in
  { token; version; client; global_tb; err_tb; errs = Atomic.make 0 }

let decr_0 a =
  let b = Disco_utils.Backoff.create () in
  let rec loop () =
    match Atomic.get a with
    | n when n > 0 && Atomic.compare_and_set a n (n - 1) -> ()
    | 0 -> ()
    | _ ->
        Disco_utils.Backoff.once b;
        loop ()
  in
  loop ()

let throttle_errors ~sw t =
  match Atomic.get t.errs with
  | n when n < 0 -> assert false
  | 0 -> ()
  | _ ->
      Token_bucket.take ~sw t.err_tb;
      decr_0 t.errs

let do_request :
    ?sw:Switch.t ->
    ?headers:(string * string) list ->
    ?body:Body.t ->
    meth:Httpaf.Method.t ->
    string ->
    t ->
    (Httpaf.Response.t * Body.t, ApiError.t) result =
 fun ?sw ?(headers = []) ?body ~meth target t ->
  let with_sw f =
    match sw with
    | None ->
        Switch.run @@ fun sw -> f sw |> Result.map @@ Pair.map_snd Body.consume
    | Some sw -> f sw
  in
  with_sw @@ fun sw ->
  Token_bucket.take ~sw t.global_tb;
  throttle_errors ~sw t;
  let target = Version.to_path t.version ^ target in
  let auth_header = ("Authorization", "Bot " ^ t.token) in
  let ua_header = ("User-Agent", user_agent) in
  let headers =
    headers @ [ auth_header; ua_header; ("Accept", "application/json") ]
  in
  L.debug (fun m -> m "making request to %s" target);
  let res, body = Client.call ~sw t.client ~headers ?body ~meth target in
  if Httpaf.Status.is_successful res.status then Ok (res, body)
  else (
    if Httpaf.Status.is_client_error res.status then Atomic.incr t.errs;
    let err_str = Body.to_string body in
    let err = ApiError.t_of_yojson (Yojson.Safe.from_string err_str) in
    L.warn (fun m ->
        m "http request failed %a@.%a" ApiError.pp err Httpaf.Response.pp_hum
          res);
    Error err)

let get ?sw ?headers url t = do_request ?sw ?headers ~meth:`GET url t

let post ?sw ?(headers = []) ?body url t =
  let headers = headers @ [ ("Content-Type", "application/json") ] in
  let body =
    body
    |> Option.map @@ fun body -> body |> Yojson.Safe.to_string |> Body.of_string
  in
  do_request ?sw ~headers ?body ~meth:`POST url t

module R = struct
  type gateway_info = {
    url : string;
    shards : int;
    session_start_limit : session_start_limit;
  }

  and session_start_limit = {
    total : int;
    remaining : int;
    reset_after : int;
    max_concurrency : int;
  }
  [@@deriving yojson, show] [@@yojson.allow_extra_fields]
end

type gateway_info =
  | Fallback of { url : string; max_concurrency : int }
  | Real of R.gateway_info

let fallback_gw_url = "wss://gateway.discord.gg"
let gateway_info = Fallback { url = fallback_gw_url; max_concurrency = 1 }

let get_gateway_info t =
  let api () =
    let open Result in
    Switch.run @@ fun sw ->
    let+ _, body = get ~sw "/gateway/bot" t in
    let body = Body.to_string body in
    L.dbg (fun m -> m "%s" body);
    let raw = Yojson.Safe.from_string body in
    R.gateway_info_of_yojson raw
  in
  match api () with
  | Ok real -> Real real
  | Error e ->
      L.warn (fun m ->
          m "Couldn't get gateway url from API, using fallback (%s): %a"
            fallback_gw_url ApiError.pp e);
      gateway_info
  | exception e ->
      let bt = Printexc.get_backtrace () in
      L.warn (fun m ->
          m "Couldn't get gateway url from API, using fallback (%s): %s@.%s"
            fallback_gw_url (Printexc.to_string e) bt);
      gateway_info
