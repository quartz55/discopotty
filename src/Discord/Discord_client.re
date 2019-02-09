module Future = BsFluture;
open Discord_types;

type t = client;

[@bs.module "discord.js"] [@bs.new] external make: unit => t = "Client";

[@bs.send.pipe: t] external login: string => Js.Promise.t(string) = "";
let login = (token, t) => Future.tryP(() => t |> login(token));

[@bs.get] [@bs.return nullable] external user: t => option(user) = "";
[@bs.get]
external voice_conns: t => collection(snowflake, voice_conn) =
  "voiceConnections";
[@bs.get]
external broadcasts: t => collection(snowflake, voice_boradcast) = "";

/* Events  */
[@bs.send.pipe: t] external _on: (string, 'cb) => t = "on";
[@bs.send.pipe: t] external _off: (string, 'handler) => t = "off";
type event =
  | Ready
  | Message(message);

let listen = t => {
  Wonka.Types.(
    Wonka.make((. {next, complete}: observerT(event)) => {
      let h_ready = () => next(Ready);
      let h_msg = msg => next(Message(msg));
      t |> _on("ready", h_ready) |> _on("message", h_msg) |> ignore;
      (.) => {
        t |> _off("ready", h_ready) |> _off("message", h_msg) |> ignore;
      };
    })
  );
};