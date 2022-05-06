include Stdint
include Containers
include Disco_utils
include Eio.Std
module Models = Disco_models

type snowflake = Models.Snowflake.t
type sf = Models.Snowflake.t
type bigstring = Bigstringaf.t
type bs = Bigstringaf.t
type no_return = |
type eio_socket = < Eio.Flow.two_way ; Eio.Flow.close >

let unreachable () = failwith "unreachable"
