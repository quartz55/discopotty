module Future = BsFluture;
module Log =
  Relog.Make({
    let namespace = "Bot";
  });

type role =
  | Admin
  | Normie
  | Bot;

let role_of_user = user =>
  switch (user |> Discord.User.bot, user |> Discord.User.username) {
  | (true, _) => Bot
  | (false, "quartz") => Admin
  | _ => Normie
  };

let parse_message = msg =>
  switch (msg |> Discord.Message.author |> role_of_user) {
  | Bot => None
  | _ => None
  };

let log_msg = msg => {
  Discord.(
    Log.debug(m => {
      let author = msg |> Message.author;
      let channel = msg |> Message.channel;
      let content = msg |> Message.content;
      let channelName =
        switch (channel |> Channel.classify) {
        | Channel.DM(_) => "DM"
        | Channel.Group(_) => "Group DM"
        | Channel.Text(textChannel) =>
          (textChannel |> TextChannel.guild |> Guild.name)
          ++ " #"
          ++ TextChannel.name(textChannel)
        | Channel.Voice(_) => "Voice"
        };
      m(
        "Message: @[%s:@]@[@@%s#%s>@]@[%s@]",
        channelName,
        author |> Discord.User.username,
        "admin",
        content,
      );
    })
  );
};

let handle_message = msg => {
  log_msg(msg);
};

[@bs.module "fs"] [@bs.val]
external read_file_sync: (string, Js.t({..})) => string = "readFileSync";
let get_token = () => {
  Some(
    read_file_sync("./token.txt", {"encoding": "utf8"}) |> Js.String.trim,
  );
};

let main = () => {
  Relog.setReporter(
    Relog.format_reporter(~level=Relog.Level.Debug, ~color=true, ()),
  );
  module Client = Discord.Client;
  let client = Client.make();
  let events = client |> Client.listen |> Wonka.share;
  events
  |> Wonka.forEach((. evt) =>
       switch (evt) {
       | Client.Ready =>
         let user =
           client
           |> Client.user
           |> Rationale.Option.toExn("Bot has no user assigned");
         Log.info(m => m("Bot is ready"));
       | Client.Message(msg) => handle_message(msg)
       }
     );

  let token =
    get_token() |> Rationale.Option.toExn("Couldn't find token.txt file");
  let%Future _ = client |> Discord.Client.login(token);
  Future.resolve();
};

main() |> Future.fork(_ => (), _ => ());