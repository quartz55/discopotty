open Discord_types;

type t = textChannel;

[@bs.get] external guild: t => guild = "";
[@bs.get] external name: t => string = "";