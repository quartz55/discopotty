open Containers
module M = Discord.Token_bucket

module U = struct
  let ns_conv =
    let conv n e = Int64.(n / (10L ** e)) in
    fun ?(unit = `ns) n ->
      match unit with
      | `ns -> n
      | `us -> conv n 3L
      | `ms -> conv n 6L
      | `s -> conv n 9L

  let ( -|> ) n u = ns_conv ~unit:u n

  let take_measure ?(unit = `ns) ~n b =
    let open Lwt.Syntax in
    let t1 = Mtime_clock.elapsed_ns () in
    let+ () = M.take ~n b in
    Int64.(Mtime_clock.elapsed_ns () - t1) |> ns_conv ~unit
end

let test_invariants () =
  let exn = Invalid_argument "can't take more than bucket's capacity" in
  let b = M.make ~capacity:10 1. in
  Alcotest.check_raises "take over capacity" exn (fun () ->
      M.try_take ~n:11 b |> ignore);
  Alcotest.(check bool) "initially full" true (M.is_full b);
  Alcotest.(check bool) "initial take" true (M.try_take ~n:10 b);
  Alcotest.(check bool) "empty take" false (M.try_take ~n:10 b)

let test_correct_waiting_time =
  QCheck.Test.make ~count:1000 ~name:"correct_waiting_time"
    QCheck.(float_range 1000. 10000.)
    (fun rate ->
      (* would take too long otherwise zzzz... *)
      QCheck.assume (rate >. 0.5);
      let ns_per_token = Int64.of_float (1e9 /. rate |> ceil) in
      let b = M.make ~capacity:1 ~init:0 rate in
      let check =
        let open Lwt.Syntax in
        let+ ns = U.take_measure ~n:1 b in
        Int64.(ns >= ns_per_token)
      in
      Lwt_main.run check)

let test_concurrent _switch () =
  let open Lwt.Syntax in
  let b = M.make ~capacity:1 ~init:0 10. in
  let t1 = Mtime_clock.elapsed_ns () in
  let* () = List.(1 -- 10) |> List.map (fun _ -> M.take b) |> Lwt.join in
  let d_ns = Int64.(Mtime_clock.elapsed_ns () - t1) in
  if Int64.(U.(d_ns -|> `s) <> 1L) then
    Alcotest.failf "expected around 1 sec waiting but got %Lds (%Ldms) (%Ldns)"
      U.(d_ns -|> `s)
      U.(d_ns -|> `ms)
      d_ns;
  Lwt.return ()

let () =
  let lwt =
    Alcotest_lwt.(
      run ~and_exit:false "Token_bucket_lwt"
        [ ("all", [ test_case "concurrent" `Quick test_concurrent ]) ])
  in
  let sync () =
    Alcotest.(
      run "Token_bucket"
        [
          ( "all",
            [
              test_case "invariants" `Quick test_invariants;
              QCheck_alcotest.to_alcotest test_correct_waiting_time;
            ] );
        ])
  in
  match Lwt_main.run lwt with _ | (exception _) -> sync ()
