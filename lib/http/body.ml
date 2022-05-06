open Eio.Std
open Containers
module L = (val Relog.logger ~namespace:"Disco_http.Body" ())

type length =
  [ `Fixed of Int64.t
  | `Chunked
  | `Error of [ `Bad_request | `Bad_gateway | `Internal_server_error ]
  | `Unknown
  | `Close_delimited ]

type data =
  [ `Empty
  | `String of string
  | `Bigstring of Bigstringaf.t
  | `Flow of Eio.Flow.source ]

type t = { length : length; data : data }

let length { length; _ } = length
let create ~length data = { length; data }
let empty = create ~length:(`Fixed 0L) `Empty
let of_flow ?(length = `Chunked) flow = create ~length (`Flow flow)

let of_string s =
  create ~length:(`Fixed Int64.(of_int @@ String.length s)) (`String s)

let of_bigstring ?(off = 0) ?len bs =
  let len = Option.get_lazy (fun () -> Bigstringaf.length bs - off) len in
  let bs = Bigstringaf.sub bs ~off ~len in
  create ~length:(`Fixed Int64.(of_int len)) (`Bigstring bs)

let to_string t =
  match t.data with
  | `Empty -> ""
  | `String s -> s
  | `Bigstring bs -> Bigstringaf.to_string bs
  | `Flow f ->
      let b = Buffer.create 1024 in
      let snk = Eio.Flow.buffer_sink b in
      Eio.Flow.copy f snk;
      Buffer.contents b

let to_bytes t =
  match t.data with
  | `Empty -> Bytes.empty
  | `String s -> Bytes.unsafe_of_string s
  | `Bigstring bs ->
      let len = Bigstringaf.length bs in
      let b = Bytes.create len in
      Bigstringaf.blit_to_bytes bs ~src_off:0 b ~dst_off:0 ~len;
      b
  | `Flow f ->
      let b = Buffer.create 1024 in
      Eio.Flow.copy f @@ Eio.Flow.buffer_sink b;
      Buffer.to_bytes b

let to_bigstring t =
  match t.data with
  | `Empty -> Bigstringaf.empty
  | `String s -> Bigstringaf.of_string ~off:0 ~len:(String.length s) s
  | `Bigstring bs -> bs
  | `Flow src ->
      let f = Faraday.create 1024 in
      let b = Bigstringaf.create 1024 in
      let cb = Cstruct.of_bigarray b in
      let rec loop () =
        match Eio.Flow.read src cb with
        | got ->
            Faraday.write_bigstring f b ~len:got;
            loop ()
        | exception End_of_file -> Faraday.serialize_to_bigstring f
      in
      loop ()

let consume t = to_bigstring t |> of_bigstring

let drain t =
  match t.data with
  | `Empty | `String _ | `Bigstring _ -> ()
  | `Flow f -> (
      let b = Cstruct.create 1024 in
      try
        while true do
          Eio.Flow.read f b |> ignore
        done
      with End_of_file -> ())

module Bodyw = Httpaf.Body.Writer
module Bodyr = Httpaf.Body.Reader

let flush_and_close b =
  L.trace (fun m -> m "closing req body");
  Bodyw.close b;
  let p, u = Promise.create () in
  Bodyw.flush b (Promise.resolve u);
  Promise.await p;
  L.trace (fun m -> m "flushed req body")

let flow_of_writer b =
  object (_ : < Eio.Flow.sink ; Eio.Flow.close ; .. >)
    method probe _ = None

    method copy src =
      let bs = Bigstringaf.create 4096 in
      let buf = Cstruct.of_bigarray bs in
      let rec loop () =
        if Bodyw.is_closed b then ()
        else
          match Eio.Flow.read src buf with
          | exception End_of_file -> ()
          | got ->
              Bodyw.schedule_bigstring b ~len:got bs;
              let p, u = Promise.create () in
              Bodyw.flush b (Promise.resolve u);
              Promise.await p;
              L.trace (fun m -> m "flow_of_writer: flushed");
              loop ()
      in
      loop ()

    method close = flush_and_close b
  end

let write t req_body =
  match t.data with
  | `Empty -> flush_and_close req_body
  | `String s ->
      Bodyw.write_string req_body s;
      flush_and_close req_body
  | `Bigstring bs ->
      Bodyw.schedule_bigstring req_body bs;
      flush_and_close req_body
  | `Flow src ->
      let dst = flow_of_writer req_body in
      Eio.Flow.copy src dst;
      flush_and_close req_body

let flow_of_reader ?on_close b =
  let on_close =
    let f = ref on_close in
    fun () ->
      match !f with
      | None -> ()
      | Some fn ->
          f := None;
          Fiber.yield ();
          fn ()
  in
  object (self : < Eio.Flow.source ; Eio.Flow.close ; .. >)
    val mutable linger = []
    val mutable eof = false
    method probe _ = None

    method read_into cbuf =
      let got, bufs = Cstruct.fillv ~src:linger ~dst:cbuf in
      linger <- bufs;
      match got with
      | got when got > 0 ->
          L.trace (fun m -> m "filled %d bytes to cbuf" got);
          got
      | 0 when eof ->
          L.trace (fun m -> m "Bodyr.eof");
          raise End_of_file
      | 0 ->
          let p, u = Promise.create () in
          let on_eof () =
            eof <- true;
            L.trace (fun m -> m "Bodyr.on_eof");
            Bodyr.close b;
            on_close ();
            Promise.resolve_error u End_of_file
          in
          let on_read bs ~off ~len =
            Promise.resolve_ok u
              (Bigstringaf.copy bs ~off ~len |> Cstruct.of_bigarray)
          in
          L.trace (fun m -> m "Bodyr.schedule read  ");
          Bodyr.schedule_read b ~on_eof ~on_read;
          let next = Promise.await_exn p in
          L.trace (fun m -> m "read %d bytes from Bodyr" (Cstruct.length next));
          linger <- linger @ [ next ];
          self#read_into cbuf
      | _ -> assert false

    method read_methods = []

    method close =
      Bodyr.close b;
      on_close ()
  end

let of_httpaf ?on_close ~length b =
  let flow = flow_of_reader ?on_close b in
  of_flow ~length (flow :> Eio.Flow.source)
