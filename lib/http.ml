open Containers

module ApiError = struct
  type t = { code : int; (* errors : errors; *)
                         message : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let code { code; _ } = code

  let message { message; _ } = message

  let pp fmt { code; message; _ } = Format.fprintf fmt "(%d: %s)" code message
end

module L = (val Relog.logger ~namespace:__MODULE__ ())

type t = { token : string; version : Versions.Http.t; client : Piaf.Client.t }

let user_agent = "discopotty (github.com/quartz55/discopotty-re, v0.0.1)"

let base_url = "https://discord.com"

let base_uri = Uri.of_string base_url

let create ?(version = Versions.Http.V8) token =
  let open Lwt_result.Syntax in
  let+ client =
    Piaf.Client.create
      ~config:
        {
          Piaf.Config.default with
          follow_redirects = true;
          allow_insecure = true;
        }
      base_uri
    |> Lwt_result.map_err Error.of_http
  in
  { token; version; client }

let do_request :
    ?headers:(string * string) list ->
    ?body:Piaf.Body.t ->
    meth:Piaf.Method.t ->
    string ->
    t ->
    (Piaf.Response.t, [> Piaf.Error.t ]) Lwt_result.t =
 fun ?(headers = []) ?body ~meth target { token; client; version; _ } ->
  let open Lwt_result.Syntax in
  let target = Versions.Http.to_path version ^ target in
  let auth_header = ("Authorization", "Bot " ^ token) in
  let ua_header = ("User-Agent", user_agent) in
  let headers =
    headers @ [ auth_header; ua_header; ("Accept", "application/json") ]
  in
  L.debug (fun m -> m "making request to %s" target);
  let* res = Piaf.Client.request client ~headers ?body ~meth target in
  if Piaf.Status.is_successful res.status then Lwt.return (Ok res)
  else
    let* err_str = Piaf.Body.to_string res.body in
    let err = ApiError.t_of_yojson (Yojson.Safe.from_string err_str) in
    L.warn (fun m ->
        m "http request failed %a@.%a" ApiError.pp err Piaf.Response.pp_hum res);
    Lwt.return (Error (`Msg ("discord http error: " ^ ApiError.message err)))

let get ?headers url t = do_request ?headers ~meth:`GET url t

let post ?(headers = []) ?body url t =
  let headers = headers @ [ ("Content-Type", "application/json") ] in
  let body =
    body |> Option.map Fun.(Yojson.Safe.to_string %> Piaf.Body.of_string)
  in
  do_request ~headers ?body ~meth:`POST url t
