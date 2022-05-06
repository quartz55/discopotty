type t = V9

let to_string = function V9 -> "9"
let to_path t = "/api/v" ^ to_string t
