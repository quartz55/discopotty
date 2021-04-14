open Globals
open Lwt.Infix

module L = (val Relog.logger ~namespace:__MODULE__ ())

module Payload = Gateway_payload
module SfMap = Map.Make (Models.Snowflake)

type t = {
  session : Session.t;
  mutable events : Events.t Lwt_stream.t;
  mutable voice_sessions : voice_session SfMap.t;
}

and voice_session = { channel_id : snowflake; conn : Voice.t; mixer : Mixer.t }

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

let disconnect t =
  SfMap.to_seq t.voice_sessions
  |> Seq.map (fun (_, { conn; _ }) -> Voice.disconnect conn)
  |> Seq.to_list |> Lwt.join
  >>= fun () ->
  t.voice_sessions <- SfMap.empty;
  Session.disconnect t.session

let connect ?http token =
  let open Lwt_result.Syntax in
  let* http =
    match http with None -> Http.create token | Some d -> Lwt_result.return d
  in
  let* url = get_gateway_url http in
  let+ session = Session.create token (Uri.of_string url) in
  let rec t = { session; events; voice_sessions }
  and events = Session.events session |> Lwt_pipe.to_stream
  and voice_sessions = SfMap.empty in
  t

let user { session; _ } = Session.user session

let events { events; _ } = events |> Lwt_pipe.of_stream

let _fork_events t = Lwt_stream.clone t.events |> Lwt_pipe.of_stream

let leave_voice ~guild_id ({ session; voice_sessions; _ } as t) =
  let open Lwt.Infix in
  match SfMap.get guild_id voice_sessions with
  | Some vs ->
      Voice.disconnect vs.conn >>= fun () ->
      t.voice_sessions <- SfMap.remove guild_id voice_sessions;
      Session.send_voice_state_update session ~self_mute:true ~self_deaf:true
        guild_id
  | None -> Lwt.return_unit

let join_voice ~guild_id ~channel_id ({ session; _ } as t) =
  let open Lwt.Syntax in
  let user = Session.user session in
  let join () =
    let* () =
      Session.send_voice_state_update session ~channel_id ~self_deaf:false
        ~self_mute:false guild_id
    in
    let is_own_vs
        { Events.Voice_state.guild_id = guild; channel_id = chan; user_id; _ } =
      match (guild, chan) with
      | Some g, Some c ->
          Models.Snowflake.(guild_id = g && channel_id = c && user_id = user.id)
      | _ -> false
    in
    let is_own_srv { Events.Voice_server_update.guild_id = guild; _ } =
      Models.Snowflake.(guild_id = guild)
    in

    let wait_for_updates =
      let rec f' ?st ?srv () =
        let* evt = Lwt_pipe.read evs in
        match ((evt : Events.t option), st, srv) with
        | Some (Voice_state_update st), _, None ->
            L.debug (fun m -> m "got voice state");
            f' ~st ?srv ()
        | Some (Voice_server_update srv), None, _ ->
            L.debug (fun m -> m "got voice server");
            f' ?st ~srv ()
        | Some (Voice_state_update st), _, Some srv
        | Some (Voice_server_update srv), Some st, _ ->
            Lwt.return (st, srv)
        | _ -> assert false
      and evs =
        _fork_events t
        |> Lwt_pipe.Reader.filter ~f:(function
             | Events.Voice_state_update st -> is_own_vs st
             | Events.Voice_server_update srv -> is_own_srv srv
             | _ -> false)
      in
      let p = f' () in
      Lwt.on_termination p (fun () ->
          L.info (fun m -> m "closing events fork");
          Lwt_pipe.close_nonblock evs);
      p
    in
    let open Lwt_result.Syntax in
    let* st, srv =
      Lwt.pick
        [
          Lwt_unix.sleep 5. |> Lwt.map (fun () -> `Timeout);
          wait_for_updates |> Lwt.map (fun o -> `Ok o);
        ]
      |> Lwt.map (function
           | `Ok o -> Ok o
           | `Timeout -> Error (`Discord "timed out waiting for update events"))
    in
    let cleanup _ =
      L.warn (fun m -> m "cleanup");
      match SfMap.get guild_id t.voice_sessions with
      | Some { mixer; _ } ->
          Mixer.destroy mixer;
          t.voice_sessions <- SfMap.remove guild_id t.voice_sessions
      | None -> ()
    in
    Voice.create ~on_destroy:cleanup ~server_id:guild_id ~user_id:user.id
      ~session_id:st.session_id ~token:srv.token srv.endpoint
  in
  let open Lwt_result.Syntax in
  match SfMap.get guild_id t.voice_sessions with
  | Some vs when Models.Snowflake.(vs.channel_id = channel_id) ->
      L.info (fun m ->
          m "there an active voice connection for channel '%Ld' already"
            vs.channel_id);
      Lwt_result.return vs
  | Some _vs ->
      L.info (fun m ->
          m "already active voice session for guild %Ld, switching channel..."
            guild_id);
      let* conn = join () in
      let+ mixer = Mixer.create conn |> Lwt_result.lift in
      let vs = { conn; channel_id; mixer } in
      t.voice_sessions <- SfMap.add guild_id vs t.voice_sessions;
      vs
  | None ->
      let* conn = join () in
      let+ mixer = Mixer.create conn |> Lwt_result.lift in
      let vs = { conn; channel_id; mixer } in
      t.voice_sessions <- SfMap.add guild_id vs t.voice_sessions;
      vs

let get_voice ~guild_id { voice_sessions; _ } =
  SfMap.get guild_id voice_sessions
