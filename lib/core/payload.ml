module L = (val Relog.logger ~namespace:__MODULE__ ())

module Raw = struct
  type json = Yojson.Safe.t

  let yojson_of_json : json -> json = Obj.magic
  let json_of_yojson : json -> json = Obj.magic

  type t = {
    op : int;
    d : json option;
    s : int option option; [@yojson.option]
    t : string option option; [@yojson.option]
  }
  [@@deriving yojson]

  let make ?d ?s ?t () op = { op; d; s; t }
end

module Dir = struct
  type recv = [ `recv ]
  type send = [ `send ]
  type 'a bidi = [< recv | send ] as 'a
end

module type Impl = sig
  type _ t

  val op : _ t -> int
  val of_raw : Raw.t -> Dir.recv t
  val to_raw : Dir.send t -> Raw.t
end

module type Intf = sig
  include Impl

  type recv = Dir.recv t
  type send = Dir.send t

  val of_json : Raw.json -> Dir.recv t
  val of_string : string -> Dir.recv t
  val of_bytes : bytes -> Dir.recv t
  val to_json : Dir.send t -> Raw.json
  val to_string : Dir.send t -> string
  val to_bytes : Dir.send t -> bytes
end

module Make : functor (I : Impl) -> Intf with type 'a t := 'a I.t =
functor
  (I : Impl)
  ->
  struct
    include I

    type recv = Dir.recv t
    type send = Dir.send t

    let of_json json = Raw.t_of_yojson json |> of_raw
    let of_string str = Yojson.Safe.from_string str |> of_json
    let of_bytes b = Bytes.unsafe_to_string b |> of_string
    let to_json t = to_raw t |> Raw.yojson_of_t
    let to_string t = to_json t |> Yojson.Safe.to_string
    let to_bytes t = to_string t |> Bytes.unsafe_of_string
  end
