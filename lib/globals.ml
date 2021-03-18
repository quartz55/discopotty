include Stdint
include Containers
module Models = Discord_models

type snowflake = Models.Snowflake.t

type bigstring = Bigstringaf.t

module Lwt_pipe = struct
  include Lwt_pipe

  let multicast ~n p =
    assert (n > 1);
    let main = create () in
    let subs = Seq.(1 --^ n >|= (fun _ -> create ()) |> to_list) in
    let running_pipes () = List.filter (fun p -> not @@ is_closed p) subs in
    let rec fwd_rec () =
      let open Lwt.Infix in
      read p >>= function
      | Some d ->
          main :: running_pipes () |> Lwt_list.exists_p (fun p -> write p d)
          >>= fun ok -> if ok then fwd_rec () else Lwt.return_unit
      | None -> Lwt.return_unit
    in
    let fwd = fwd_rec () in
    keep main fwd;
    List.iter (fun p -> link_close p ~after:main) subs;
    Lwt.on_termination fwd (fun () -> close_nonblock main);
    (main, subs)

  let fork p =
    let p, sibs = multicast ~n:2 p in
    (p, List.hd sibs)
end
