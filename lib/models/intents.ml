type t = int [@@deriving yojson]

let make = List.fold_left (fun out intent -> out lor intent) 0
let add i t = t lor i
let rm i t = lnot i land t
let guilds = 1 lsl 0
let guild_members = 1 lsl 1
let guild_bans = 1 lsl 2
let guild_emojis = 1 lsl 3
let guild_integrations = 1 lsl 4
let guild_webhooks = 1 lsl 5
let guild_invites = 1 lsl 6
let guild_voice_states = 1 lsl 7
let guild_presences = 1 lsl 8
let guild_messages = 1 lsl 9
let guild_message_reactions = 1 lsl 10
let guild_message_typing = 1 lsl 11
let direct_messages = 1 lsl 12
let direct_message_reactions = 1 lsl 13
let direct_message_typing = 1 lsl 14
let all = (1 lsl 15) - 1

let all_unprivileged =
  List.fold_left (fun is i -> rm i is) all [ guild_presences; guild_members ]

let pp fmt t =
  let out =
    let rec aux acc d miss =
      match (d, miss) with
      | 0, 0 -> String.concat "" acc
      | 0, n when n > 0 -> String.make n '0' ^ String.concat "" acc
      | _, n -> aux (string_of_int (d land 1) :: acc) (d lsr 1) (n - 1)
    in
    aux [] t 15
  in
  Format.fprintf fmt "%s" out

let show = Format.asprintf "%a" pp
