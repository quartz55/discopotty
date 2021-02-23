open Globals

module Ready = struct
  type t = {
    v : int;
    (* user : Models.User.t; *)
    (* guilds : Models.Guild.t list; *)
    session_id : string;
    shard : (int * int) option; [@yojson.option]
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

type t =
  | Ready of Ready.t
  | MessageCreate of Models.Message.t
  | Resumed
  | Unsupported of string * Yojson.Safe.t option

let of_name name data =
  match (name, data) with
  | "READY", Some d -> Ready (Ready.t_of_yojson d)
  | "MESSAGE_CREATE", Some d -> MessageCreate (Models.Message.t_of_yojson d)
  | "RESUMED", _ -> Resumed
  | other, d -> Unsupported (other, d)
