open Containers
module Flow = Eio.Flow
module L = (val Relog.logger ~namespace:__MODULE__ ())

exception Tls_alert of Tls.Packet.alert_type
exception Tls_failure of Tls.Engine.failure
exception Closed

let () =
  Printexc.register_printer (function
    | Tls_alert typ ->
        Some ("TLS alert from peer: " ^ Tls.Packet.alert_type_to_string typ)
    | Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
    | Closed -> Some "TLS closed"
    | _ -> None)

type t = {
  role : [ `Server | `Client ];
  flow : < Flow.two_way ; Flow.close >;
  mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
  mutable linger : Cstruct.t list;
}

let () = Mirage_crypto_rng_unix.initialize ()

let check_active t =
  match t.state with
  | `Eof -> raise End_of_file
  | `Error exn -> raise exn
  | `Active tls -> tls

let check_write t res =
  match (t.state, res) with
  | _, Ok () -> ()
  | `Active _, Error e ->
      t.state <- `Error e;
      Flow.close t.flow;
      raise e
  | _, Error e -> raise e

let copy src dst =
  match Flow.copy src dst with () -> Ok () | exception ex -> Error ex

let read_react t =
  let handle tls buf =
    match Tls.Engine.handle_tls tls buf with
    | Ok (res, `Response resp, `Data data) ->
        let state' =
          match res with
          | `Ok tls ->
              L.trace (fun m -> m "tls still active");
              `Active tls
          | `Eof ->
              L.trace (fun m -> m "tls eof");
              `Eof
          | `Alert alert ->
              L.trace (fun m -> m "tls alert");
              `Error (Tls_alert alert)
        in
        t.state <- state';
        Option.iter
          (fun buf ->
            copy (Flow.cstruct_source [ buf ]) t.flow |> check_write t)
          resp;
        (match res with `Ok _ -> () | _ -> Flow.close t.flow);
        L.trace (fun m ->
            m "data@.%a"
              (Option.pp (Hxd_string.pp Hxd.default))
              (Option.map Cstruct.to_string data));
        data
    | Error (fail, `Response resp) ->
        L.trace (fun m -> m "tls failed");
        let ex = Tls_failure fail in
        t.state <- `Error ex;
        (try Flow.copy (Flow.cstruct_source [ resp ]) t.flow with _ -> ());
        Flow.close t.flow;
        raise ex
  in
  check_active t |> ignore;
  let cbuf = Cstruct.create 4096 in
  (* this is hanging everything for some reason *)
  match Flow.read t.flow cbuf with
  | got ->
      L.trace (fun m -> m "got %dB" got);
      let tls = check_active t in
      let cbuf = Cstruct.sub cbuf 0 got in
      handle tls cbuf
  | exception (End_of_file as ex) ->
      t.state <- `Eof;
      raise ex
  | exception ex ->
      t.state <- `Error ex;
      raise ex

let rec read_into t buf =
  let got, bufs = Cstruct.fillv ~src:t.linger ~dst:buf in
  t.linger <- bufs;
  if got > 0 then (
    L.trace (fun m -> m "tls: read %dB into buf" got);
    got)
  else
    match read_react t with
    | None -> read_into t buf
    | Some next ->
        t.linger <- t.linger @ [ next ];
        read_into t buf

let writev t bufs =
  match t.state with
  | `Eof -> raise Closed
  | `Error e -> raise e
  | `Active tls -> (
      match Tls.Engine.send_application_data tls bufs with
      | Some (tls, answer) ->
          t.state <- `Active tls;
          L.trace (fun m -> m "tls writev %dB" (Cstruct.length answer));
          copy (Flow.cstruct_source [ answer ]) t.flow |> check_write t
      | None ->
          (* "Impossible" due to handshake draining. *)
          assert false)

let write t buf = writev t [ buf ]

let close t =
  match t.state with
  | `Active tls ->
      L.trace (fun m -> m "close_notify tls");
      t.state <- `Eof;
      let _, buf = Tls.Engine.send_close_notify tls in
      (* XXX: need a switch here *)
      let _ = copy (Flow.cstruct_source [ buf ]) t.flow in
      L.trace (fun m -> m "closing underlying flow");
      Flow.shutdown t.flow `Send;
      L.trace (fun m -> m "closed underlying flow")
  | _ -> L.trace (fun m -> m "already closed")

(*
 * XXX bad XXX
 * This is a point that should particularly be protected from concurrent r/w.
 * Doing this before a `t` is returned is safe; redoing it during rekeying is
 * not, as the API client already sees the `t` and can mistakenly interleave
 * writes while this is in progress.
 * *)
let rec drain_handshake t =
  match t.state with
  | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> ()
  | _ ->
      (* read_react re-throws *)
      let mbuf = read_react t in
      t.linger <- Option.to_list mbuf @ t.linger;
      drain_handshake t

let wrap t =
  object (_ : < Eio.Generic.t ; Flow.source ; Flow.sink ; Flow.close ; .. >)
    method probe _ = None
    method read_into buf = read_into t buf
    method read_methods = [] (* TODO: this would be faster *)

    method copy src =
      let buf = Cstruct.create 4096 in
      let rec loop () =
        match Flow.read src buf with
        | exception End_of_file -> ()
        | got ->
            (* XXX: wrap errors? *)
            write t (Cstruct.sub buf 0 got);
            loop ()
      in
      loop ()

    method epoch =
      match t.state with
      | `Eof | `Error _ -> Error ()
      | `Active tls -> (
          match Tls.Engine.epoch tls with
          | `InitialEpoch -> assert false (* `drain_handshake` invariant. *)
          | `Epoch e -> Ok e)

    method reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) () =
      match t.state with
      | `Eof -> raise End_of_file
      | `Error e -> raise e
      | `Active tls -> (
          match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
          | None ->
              (* XXX make this impossible to reach *)
              invalid_arg "Renegotiation already in progress"
          | Some (tls', buf) ->
              if drop then t.linger <- [];
              t.state <- `Active tls';
              copy (Flow.cstruct_source [ buf ]) t.flow |> fun _ ->
              drain_handshake t)

    method key_update ?request () =
      match t.state with
      | `Eof -> raise Closed
      | `Error e -> raise e
      | `Active tls -> (
          match Tls.Engine.key_update ?request tls with
          | Error _ -> invalid_arg "Key update failed"
          | Ok (tls', buf) ->
              t.state <- `Active tls';
              copy (Flow.cstruct_source [ buf ]) t.flow |> check_write t)

    method close = close t
    method shutdown = function `All | `Send -> close t | `Receive -> ()
  end

let client_of_flow ~sw conf ?host flow =
  let conf' =
    match host with None -> conf | Some host -> Tls.Config.peer conf host
  in
  let tls, init = Tls.Engine.client conf' in
  let t = { role = `Client; flow; state = `Active tls; linger = [] } in
  copy (Flow.cstruct_source [ init ]) flow |> fun _ ->
  drain_handshake t;
  let flow = wrap t in
  Eio.Switch.on_release sw (fun () -> Flow.close flow);
  flow

let server_of_flow ~sw conf flow =
  let t =
    {
      role = `Server;
      flow;
      state = `Active (Tls.Engine.server conf);
      linger = [];
    }
  in
  drain_handshake t;
  let flow = wrap t in
  Eio.Switch.on_release sw (fun () -> Flow.close flow);
  flow
