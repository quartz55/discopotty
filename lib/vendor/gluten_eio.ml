open Eio.Std
module L = (val Relog.logger ~namespace:__MODULE__ ())

module type IO = sig
  type socket
  type addr

  val read :
    ?l:Relog.logger ->
    socket ->
    Bigstringaf.t ->
    off:int ->
    len:int ->
    [ `Eof | `Ok of int ]
  (** The region [(off, off + len)] is where read bytes can be written to *)

  val writev :
    ?l:Relog.logger ->
    socket ->
    Faraday.bigstring Faraday.iovec list ->
    [ `Closed | `Ok of int ]

  val shutdown_send : ?l:Relog.logger -> socket -> unit
  val shutdown_receive : ?l:Relog.logger -> socket -> unit
  val close : ?l:Relog.logger -> socket -> unit
end

module type Server = sig
  type socket
  type addr

  val create_upgradable_connection_handler :
    ?l:Relog.logger ->
    read_buffer_size:int ->
    protocol:'t Gluten.runtime ->
    create_protocol:(('reqd -> unit) -> 't) ->
    request_handler:(addr -> 'reqd Gluten.Server.request_handler) ->
    sw:Switch.t ->
    addr ->
    socket ->
    unit

  val create_connection_handler :
    ?l:Relog.logger ->
    read_buffer_size:int ->
    protocol:'t Gluten.runtime ->
    't ->
    sw:Switch.t ->
    addr ->
    socket ->
    unit
end

module type Client = sig
  type t
  type socket

  val create :
    ?l:Relog.logger ->
    sw:Switch.t ->
    read_buffer_size:int ->
    protocol:'t Gluten.runtime ->
    't ->
    socket ->
    t

  val upgrade : t -> Gluten.impl -> unit
  val shutdown : t -> unit
  val is_closed : t -> bool
end

module Qe = Ke.Rke.Weighted

module Buffer = struct
  type t = (char, Bigarray.int8_unsigned_elt) Qe.t

  let create capacity =
    let queue, _ = Qe.create ~capacity Bigarray.char in
    queue

  let get t f =
    Qe.compress t;
    match Qe.N.peek t with
    | [] -> f Bigstringaf.empty ~off:0 ~len:0
    | [ slice ] ->
        assert (Bigstringaf.length slice = Qe.length t);
        let n = f slice ~off:0 ~len:(Bigstringaf.length slice) in
        Qe.N.shift_exn t n;
        n
    | _ :: _ ->
        (* Should never happen because we compress on every `put`, and therefore
         * the Queue never wraps around to the beginning. *)
        assert false

  let blit _src _off _dst _dst_off _len = ()
  let linger t = Qe.length t

  let put t f =
    Qe.compress t;
    let buffer = Qe.unsafe_bigarray t in
    match f buffer ~off:(Qe.length t) ~len:(Qe.available t) with
    | `Eof -> `Eof
    | `Ok n as ret ->
        (* Increment the offset, without making a copy *)
        let (_ : ('a, 'b) Qe.N.bigarray list) =
          Qe.N.push_exn t ~blit ~length:(fun _ -> n) buffer
        in
        ret
end

module IO_loop = struct
  let start :
      type t fd.
      (module IO with type socket = fd) ->
      (module Gluten.RUNTIME with type t = t) ->
      ?l:Relog.logger ->
      t ->
      read_buffer_size:int ->
      fd ->
      unit =
   fun (module Io) (module Runtime) ?(l = (module L)) t ~read_buffer_size socket ->
    let module L = (val l) in
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop ?(linger = None) () =
      match Runtime.next_read_operation t with
      | `Read -> (
          L.trace (fun m -> m "read_loop: read");
          let res =
            match linger with
            | Some r ->
                Fiber.yield ();
                r
            | None -> Buffer.put read_buffer (Io.read ~l socket)
          in
          match res with
          | `Eof ->
              L.trace (fun m -> m "read_loop: read EOF");
              let n =
                Buffer.get read_buffer @@ fun bigstring ~off ~len ->
                Runtime.read_eof t bigstring ~off ~len
              in
              L.trace (fun m -> m "read_loop: runtime read EOF %d" n);
              let linger =
                if n <> 0 && Buffer.linger read_buffer > 0 then Some `Eof
                else None
              in
              read_loop ~linger ()
          | `Ok n ->
              L.trace (fun m -> m "read_loop: read %d" n);
              let n2 =
                Buffer.get read_buffer @@ fun bigstring ~off ~len ->
                L.trace (fun m -> m "read_loop: buffer get %d" len);
                Runtime.read t bigstring ~off ~len
              in
              L.trace (fun m -> m "read_loop: runtime read %d" n2);
              let l = Buffer.linger read_buffer in
              let linger = if n2 <> 0 && l > 0 then Some (`Ok l) else None in
              read_loop ~linger ())
      | `Yield ->
          let p, u = Promise.create () in
          L.trace (fun m -> m "read_loop: yield");
          Runtime.yield_reader t (Promise.resolve u);
          Promise.await p;
          read_loop ()
      | `Close ->
          L.trace (fun m -> m "read_loop: close");
          Io.shutdown_receive ~l socket
    in
    let rec write_loop () =
      match Runtime.next_write_operation t with
      | `Write io_vectors ->
          L.trace (fun m -> m "write_loop: write");
          Io.writev ~l socket io_vectors |> Runtime.report_write_result t;
          write_loop ()
      | `Yield ->
          let p, u = Promise.create () in
          L.trace (fun m -> m "write_loop: yield");
          Runtime.yield_writer t (Promise.resolve u);
          Promise.await p;
          write_loop ()
      | `Close n ->
          L.trace (fun m -> m "write_loop: close %d" n);
          Io.shutdown_send ~l socket
    in
    let run () =
      Fiber.both
        (fun () ->
          try read_loop () with
          | Eio.Cancel.Cancelled _ as e -> raise e
          | exn ->
              L.warn (fun m -> m "error reading: %a" Fmt.exn exn);
              Runtime.report_exn t exn)
        (fun () ->
          try write_loop () with
          | Eio.Cancel.Cancelled _ as e -> raise e
          | exn ->
              L.warn (fun m -> m "error writing: %a" Fmt.exn exn);
              Runtime.report_exn t exn)
    in
    Fun.protect
      ~finally:(fun () ->
        L.trace (fun m -> m "io_loop: done");
        Io.close ~l socket)
      run
end

module MakeServer (Io : IO) :
  Server with type socket = Io.socket and type addr = Io.addr = struct
  module Server = Gluten.Server

  type socket = Io.socket
  type addr = Io.addr

  let create_connection_handler ?l ~read_buffer_size ~protocol connection ~sw
      _client_addr socket =
    let connection = Server.create ~protocol connection in
    Switch.on_release sw (fun () -> Server.shutdown connection);
    IO_loop.start
      (module Io)
      (module Server)
      ?l connection ~read_buffer_size socket

  let create_upgradable_connection_handler ?l ~read_buffer_size ~protocol
      ~create_protocol ~request_handler ~sw client_addr socket =
    let connection =
      Server.create_upgradable ~protocol ~create:create_protocol
        (request_handler client_addr)
    in
    Switch.on_release sw (fun () -> Server.shutdown connection);
    IO_loop.start
      (module Io)
      (module Server)
      ?l connection ~read_buffer_size socket
end

module MakeClient (Io : IO) : Client with type socket = Io.socket = struct
  module Client_connection = Gluten.Client

  type socket = Io.socket

  type t = {
    connection : Client_connection.t;
    socket : socket;
    logger : Relog.logger;
  }

  let create ?l ~sw ~read_buffer_size ~protocol t socket =
    let namespace =
      Option.bind l (fun (l : Relog.logger) ->
          let module L = (val l) in
          L.namespace)
      |> function
      | Some ns -> String.concat "." [ "Client"; ns ]
      | None -> "Client"
    in
    let fields =
      Option.map
        (fun (l : Relog.logger) ->
          let module L = (val l) in
          L.fields |> Relog.Fields.to_seq |> List.of_seq)
        l
    in
    let module L = (val Relog.child ~namespace ?fields (module L)) in
    let connection = Client_connection.create ~protocol t in
    Fiber.fork ~sw (fun () ->
        IO_loop.start
          (module Io)
          (module Client_connection)
          ~l:(module L)
          connection ~read_buffer_size socket);
    { connection; socket; logger = (module L) }

  let upgrade t protocol =
    Client_connection.upgrade_protocol t.connection protocol

  let shutdown t =
    let module L = (val t.logger) in
    L.trace (fun m -> m "shutting client down");
    Client_connection.shutdown t.connection

  let is_closed t = Client_connection.is_closed t.connection
end

type eio_socket = < Eio.Flow.two_way ; Eio.Flow.close >

module Eio_io :
  IO with type socket = eio_socket and type addr = Eio.Net.Sockaddr.stream =
struct
  type socket = eio_socket
  type addr = Eio.Net.Sockaddr.stream

  let read ?(l : Relog.logger = (module L)) socket bigstring ~off ~len =
    let module L = (val l) in
    L.trace (fun m -> m "reading up to %dB from eio socket" (len - off));
    let buf = Cstruct.of_bigarray bigstring ~off ~len in
    match Eio.Flow.read socket buf with
    | got ->
        L.trace (fun m -> m "read %dB" got);
        `Ok got
    | (exception Eio.Net.Connection_reset _) | (exception End_of_file) ->
        L.trace (fun m -> m "EOF");
        `Eof

  let writev ?(l : Relog.logger = (module L)) socket iovecs =
    let module L = (val l) in
    L.trace (fun m -> m "writting %d iovecs to eio socket" (List.length iovecs));
    let source =
      iovecs
      |> List.map (fun { Faraday.buffer; off; len } ->
             Cstruct.of_bigarray buffer ~off ~len)
      |> Eio.Flow.cstruct_source
    in
    try
      Eio.Flow.copy source socket;
      let n =
        iovecs |> List.fold_left (fun n { Faraday.len; _ } -> n + len) 0
      in
      L.trace (fun m -> m "wrote %dB to eio socket" n);
      `Ok n
    with
    | Unix.Unix_error (Unix.EPIPE, _, _) | Eio_luv.Low_level.Luv_error `EPIPE ->
      `Closed

  let shutdown socket cmd =
    try Eio.Flow.shutdown socket cmd
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _)
    | Eio_luv.Low_level.Luv_error `ENOTCONN
    ->
      ()

  let shutdown_send ?(l : Relog.logger = (module L)) socket =
    let module L = (val l) in
    L.trace (fun m -> m "shutdown_send eio socket");
    shutdown socket `Send

  let shutdown_receive ?(l : Relog.logger = (module L)) socket =
    let module L = (val l) in
    L.trace (fun m -> m "shutdown_receive eio socket");
    try shutdown socket `Receive with Failure _ -> ()

  let close ?(l : Relog.logger = (module L)) socket =
    let module L = (val l) in
    L.trace (fun m -> m "closing eio socket");
    shutdown socket `All
end

module Client = MakeClient (Eio_io)
module Server = MakeServer (Eio_io)
