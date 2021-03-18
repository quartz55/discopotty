module Http = struct
  type t = V8

  let to_string = function V8 -> "8"

  let to_path t = "/api/v" ^ to_string t
end

module Gateway = struct
  type t = V8

  let to_string = function V8 -> "8"

  let to_query t = ("v", [ to_string t ])
end

module Voice = struct
  type t = V4

  let to_string = function V4 -> "4"

  let to_query t = ("v", [ to_string t ])
end
