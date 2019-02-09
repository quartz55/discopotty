module Future = BsFluture;
open Discord_types;

type t = channel;
type classifiedChannel =
  | DM(dmChannel)
  | Group(groupDMChannel)
  | Text(textChannel)
  | Voice(voiceChannel);

[@bs.get] external client: t => client = "";
[@bs.get] external created_at: t => Js.Date.t = "createdAt";
[@bs.get] external created_ts: t => float = "createdTimestamp";
[@bs.get] external id: t => snowflake = "";
/* type: 'dm' | 'group' | 'text' | 'voice', */
[@bs.get] external channel_type: t => string = "type";
[@bs.send] external delete: t => Js.Promise.t(t) = "";
let classify = t =>
  switch (t |> channel_type) {
  | "dm" => DM(Obj.magic(t))
  | "group" => Group(Obj.magic(t))
  | "text" => Text(Obj.magic(t))
  | "voice" => Voice(Obj.magic(t))
  | x => failwith("Unknown channelType " ++ x)
  };