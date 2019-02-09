open Discord_types;

type t = message;

[@bs.get] external author: t => user = "";
[@bs.get] external channel: t => channel = "";
[@bs.get] external content: t => string = "";
[@bs.get] external id: t => snowflake = "";