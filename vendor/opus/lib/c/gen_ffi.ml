let c_headers = {|
#include <opus.h>
#include <opus_multistream.h>
|}

let write_stub fname =
  let oc = open_out_bin fname in
  let ext = Filename.extension fname in
  let format = Format.formatter_of_out_channel oc in
  let fn = match ext with
  | "" | ".ml" -> Cstubs.write_ml
  | ".c"  ->
    Format.fprintf format "%s@\n" c_headers;
    Cstubs.write_c
  | _ -> failwith "invalid stub extension: must be either `.c` or `.ml`(default)"
  in
  fn ~concurrency:Cstubs.unlocked format ~prefix:"opus_stubs" (module Ffi_stubs.Def);
  Format.pp_print_flush format ();
  close_out oc

let () =
  Sys.argv |> Array.to_list |> List.tl |> List.iter write_stub
