let sha1 s =
  s |> Cstruct.of_string |> Mirage_crypto.Hash.SHA1.digest |> Cstruct.to_string

module Client (Client_runtime : Gluten_eio.Client) = struct
  type t = Client_runtime.t
  type socket = Client_runtime.socket

  let connect ?l ?(config = Httpaf.Config.default) ~sw ~nonce ~host ~port
      ~resource ~error_handler ~websocket_handler socket =
    let headers =
      Httpaf.Headers.of_list
        [ ("host", String.concat ":" [ host; string_of_int port ]) ]
    in
    let connection =
      Websocketaf.Client_connection.connect ~nonce ~headers ~sha1 ~error_handler
        ~websocket_handler resource
    in
    Client_runtime.create ?l ~sw ~read_buffer_size:config.read_buffer_size
      ~protocol:(module Websocketaf.Client_connection)
      connection socket

  let is_closed t = Client_runtime.is_closed t
  let shutdown t = Client_runtime.shutdown t
end
