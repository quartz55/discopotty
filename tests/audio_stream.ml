(* The tests
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string) "same string" "foobar" (To_test.str_concat ["foo"; "bar"])

let test_list_concat () =
  Alcotest.(check (list int)) "same lists" [1; 2; 3] (To_test.list_concat [1] [2; 3])

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "string-case", [
          test_case "Lower case"     `Quick test_lowercase;
          test_case "Capitalization" `Quick test_capitalize;
        ];
      "string-concat", [ test_case "String mashing" `Quick test_str_concat  ];
      "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ];
    ] *)