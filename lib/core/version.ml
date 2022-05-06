type t = V9

let to_string = function V9 -> "9"
let to_query t = ("v", [ to_string t ])
