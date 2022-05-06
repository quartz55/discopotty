module Server (Server_runtime : Gluten_eio.Server) = struct
  type socket = Server_runtime.socket

  let create_connection_handler ?(config = Httpaf.Config.default)
      ~request_handler ~error_handler ~sw client_addr socket =
    let create_connection =
      Httpaf.Server_connection.create ~config
        ~error_handler:(error_handler client_addr)
    in
    Server_runtime.create_upgradable_connection_handler
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module Httpaf.Server_connection)
      ~create_protocol:create_connection ~request_handler ~sw client_addr socket
end

module Client (Client_runtime : Gluten_eio.Client) = struct
  type socket = Client_runtime.socket
  type runtime = Client_runtime.t
  type t = { connection : Httpaf.Client_connection.t; runtime : runtime }

  let create_connection ?l ?(config = Httpaf.Config.default) ~sw socket =
    let connection = Httpaf.Client_connection.create ~config in
    let runtime =
      Client_runtime.create ?l ~sw ~read_buffer_size:config.read_buffer_size
        ~protocol:(module Httpaf.Client_connection)
        connection socket
    in
    { runtime; connection }

  let request t = Httpaf.Client_connection.request t.connection
  let shutdown t = Client_runtime.shutdown t.runtime
  let is_closed t = Client_runtime.is_closed t.runtime
  let upgrade t protocol = Client_runtime.upgrade t.runtime protocol
end
