open Containers

let work () =
  let sine_stream = Discord.Audio_stream.Gen.sine ~freq:1000 3. in
  let buf = Bigstringaf.create (960 * 2 * 4) in
  let fill_buff frame =
    let f = Faraday.create (1024 * 4) in
    Seq.(
      0 --^ Bigarray.Array1.dim frame
      |> iter (fun i -> Faraday.LE.write_float f frame.{i}));
    Faraday.serialize_to_bigstring f
  in
  sine_stream
  |> Lwt_pipe.Reader.iter_s ~f:(fun frame ->
         Lwt_io.write_from_exactly_bigstring Lwt_io.stdout (fill_buff frame) 0
           (Bigstringaf.length buf))

(* let () = Lwt_main.run (work ())*)

module Mpmc = Dp_utils.Mpmc

let () =
  let source, sink = Mpmc.create () in
  let sink2 = Mpmc.Sink.(clone sink |> map ~f:(fun i -> i * 2)) in
  let _cancel =
    Mpmc.Sink.iter sink2 ~f:(function
      | Some v -> Printf.printf "(sink 2) %d\n%!" v
      | None -> Printf.printf "(sink 2) dropped\n%!")
  in
  let _cancel =
    Mpmc.Sink.iter sink ~f:(function
      | Some v -> Printf.printf "(sink 1) %d\n%!" v
      | None -> Printf.printf "(sink 1) dropped\n%!")
  in
  Seq.(1 -- 10 |> iter (fun v -> Mpmc.Source.write source v |> ignore));
  Seq.(1 -- 5 |> iter (fun v -> Mpmc.Source.write source v |> ignore));
  Mpmc.Source.close source
