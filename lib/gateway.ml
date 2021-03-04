open Globals

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Payload = Gateway_payload

type t = {
  session : Session.t;
  events : Events.t Lwt_pipe.Reader.t;
  user : Models.User.t;
}

let get_gateway_url http =
  let api () =
    let open Lwt_result.Syntax in
    let* res = Http.get "/gateway/bot" http in
    let* body = Piaf.Body.to_string res.body in
    Result.guard_str (fun () ->
        let raw = Yojson.Safe.from_string body in
        Yojson.Safe.Util.(raw |> member "url" |> to_string))
    |> Result.map_err (fun e -> `Msg e)
    |> Lwt.return
  in
  api ()
  |> Lwt.map (function
       | Ok url -> url
       | Error e ->
           L.err (fun m ->
               m "Couldn't get gateway url from API, using fallback: %s@."
                 (Piaf.Error.to_string e));
           "wss://gateway.discord.gg")
  |> Lwt_result.ok

let connect ?http token =
  let open Lwt_result.Syntax in
  let* http =
    match http with None -> Http.create token | Some d -> Lwt_result.return d
  in
  let* url = get_gateway_url http in
  let+ session, user = Session.create token (Uri.of_string url) in
  let rec t = { session; events; user } and events = Session.events session in
  t

let disconnect { session; _ } = Session.disconnect session

let user { user; _ } = user

let events { events; _ } = events

let join_voice ~guild_id ~channel_id { session; user; _ } =
  let open Lwt.Syntax in
  let* () =
    Session.send_voice_state_update session ~channel_id ~self_deaf:false
      ~self_mute:false guild_id
  in
  let is_own_vs
      { Events.VoiceState.guild_id = guild; channel_id = chan; user_id; _ } =
    match (guild, chan) with
    | Some g, Some c ->
        Models.Snowflake.(guild_id = g && channel_id = c && user_id = user.id)
    | _ -> false
  in
  let is_own_srv { Events.VoiceServerUpdate.guild_id = guild; _ } =
    Models.Snowflake.(guild_id = guild)
  in

  let wait_for_updates =
    let rec f' ?st ?srv () =
      let* evt = Lwt_pipe.read evs in
      match ((evt : Events.t option), st, srv) with
      | Some (VoiceStateUpdate st), _, None ->
          L.debug (fun m -> m "got voice state");
          f' ~st ?srv ()
      | Some (VoiceServerUpdate srv), None, _ ->
          L.debug (fun m -> m "got voice server");
          f' ?st ~srv ()
      | Some (VoiceStateUpdate st), _, Some srv
      | Some (VoiceServerUpdate srv), Some st, _ ->
          Lwt_pipe.close_nonblock evs;
          Lwt.return (st, srv)
      | _ -> assert false
    and evs =
      let p = Lwt_pipe.create () in
      Lwt_pipe.connect ~ownership:`InOwnsOut (Session.events session) p;
      Lwt_pipe.Reader.filter p ~f:(function
        | Events.VoiceStateUpdate st -> is_own_vs st
        | Events.VoiceServerUpdate srv -> is_own_srv srv
        | _ -> false)
    in
    f' ()
  in
  let* o =
    Lwt.pick
      [
        Lwt_unix.sleep 5. |> Lwt.map (Fun.const `Timeout);
        wait_for_updates |> Lwt.map (fun o -> `Ok o);
      ]
  in
  let st, srv =
    match o with
    | `Ok o -> o
    | `Timeout -> failwith "timed out waiting for update events"
  in
  let+ voice_conn =
    Voice.create ~server_id:guild_id ~user_id:user.id ~session_id:st.session_id
      ~token:srv.token srv.endpoint
  in
  match voice_conn with Ok _ -> () | Error e -> failwith (Error.to_string e)

let leave_voice ~guild_id { session; _ } =
  Session.send_voice_state_update session ~self_mute:true ~self_deaf:true
    guild_id
