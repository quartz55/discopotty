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
