open Discord_types;

type t = user;
[@bs.get] external bot: t => bool = "";
[@bs.get] external username: t => string = "";