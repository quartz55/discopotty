open! Globals
module L = (val Relog.logger ~namespace:__MODULE__ ())
module F = Relog.Field
module Client = Http_client

let user_agent = "discopotty (github.com/quartz55/discopotty-re, v0.0.1)"

(* TODO @quartz55 *)
module ApiError = struct
  type t = { code : int; (* errors : errors; *) message : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let code { code; _ } = code
  let message { message; _ } = message
  let pp fmt { code; message; _ } = Format.fprintf fmt "(%d: %s)" code message
end

type t = { token : string; version : Versions.Http.t; client : Client.t }

let create ~sw ~net ?(version = Versions.Http.V8) token =
  let client = Client.create ~sw ~net () in
  { token; version; client }

let do_request :
    ?headers:(string * string) list ->
    ?body:bs ->
    meth:Httpaf.Method.t ->
    string ->
    t ->
    (Httpaf.Response.t * bs, ApiError.t) result =
 fun ?(headers = []) ?body ~meth target { token; client; version; _ } ->
  let target = Versions.Http.to_path version ^ target in
  let auth_header = ("Authorization", "Bot " ^ token) in
  let ua_header = ("User-Agent", user_agent) in
  let headers =
    headers @ [ auth_header; ua_header; ("Accept", "application/json") ]
  in
  L.debug (fun m -> m "making request to %s" target);
  let res, body = Client.call client ~headers ?body ~meth target in
  if Httpaf.Status.is_successful res.status then Ok (res, body)
  else
    let err_str = Bigstringaf.to_string body in
    let err = ApiError.t_of_yojson (Yojson.Safe.from_string err_str) in
    L.warn (fun m ->
        m "http request failed %a@.%a" ApiError.pp err Httpaf.Response.pp_hum
          res);
    Error err

let get ?headers url t = do_request ?headers ~meth:`GET url t

let post ?(headers = []) ?body url t =
  let headers = headers @ [ ("Content-Type", "application/json") ] in
  let body =
    body
    |> Option.map @@ fun body ->
       let str = body |> Yojson.Safe.to_string in
       Bigstringaf.of_string ~off:0 ~len:(String.length str) str
  in
  do_request ~headers ?body ~meth:`POST url t

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
    let+ _, body = get "/gateway/bot" t in
    let body = Bigstringaf.to_string body in
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
      L.warn (fun m ->
          m "Couldn't get gateway url from API, using fallback (%s): %s@.%s"
            fallback_gw_url (Printexc.to_string e)
            (Printexc.get_backtrace ()));
      gateway_info
