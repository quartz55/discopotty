open Containers

module P = struct
  open Angstrom

  let is_ws = function ' ' | '\t' | '\n' -> true | _ -> false
  let ws = skip_while is_ws

  let cmd prefix =
    let* name = ws *> string prefix *> take_till is_ws <* ws in
    let+ args = many any_char <* end_of_input >>| String.of_list in
    (name, args)
end

let parse = Angstrom.parse_string ~consume:Angstrom.Consume.All

let of_message ~prefix msg =
  match parse (P.cmd prefix) msg with Error _ -> None | Ok cmd -> Some cmd
