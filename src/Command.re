module L =
  Relog.Make({
    let namespace = "Command";
  });
module Info = {
  type pos_kind = {
    start: int,
    len: option(int),
  };

  let pos = (~start, ~len) => {start, len};
  let pos_start = p => p.start;
  let pos_len = p => p.len;

  type arg = {
    id: int,
    absent: unit,
    pos: pos_kind,
  };

  module Arg = {
    type t = arg;
    let compare = (a0, a1) => compare(a0.id, a1.id);
  };
  module Args = Set.Make(Arg);
  type args = Args.t;

  type term_info = {name: string};

  type term = {
    info: term_info,
    args,
  };

  let term_add_args = (args, t) => {...t, args: Args.union(args, t.args)};

  type eval = {
    term,
    main: term,
    choices: list(term),
  };

  let eval = (~term, ~main, ~choices) => {term, main, choices};
};

module Arg = {
  type parser('a) = string => Belt.Result.t('a, string);
  type conv('a) = parser('a);
  let conv = parse => parse;
  let (&) = (f, x) => f(x);

  let pos = (k, parse, v, a) => {
    ();
  };
  let value = a => a;
};

module Runner = {
  type t;

  let parse_str = (~prefix="~", input) => {
    let sub_str = (s0, s1) => {
      let len = String.length(s1);
      String.sub(s0, len, String.length(s0) - len);
    };
    switch (Js.String.(startsWith(prefix, prefix) && trim(input) != prefix)) {
    | false => None
    | true =>
      let cmd = sub_str(input, prefix);
      switch (String.index(cmd, ' ')) {
      | (-1) => Some((cmd, []))
      | first_space =>
        let re = [%bs.re {|/(?:"(.+?)" |'(.+?)' |(.+?) )/|}];
        let rec p' = (args, to_parse) => {
          switch (Js.Re.exec(to_parse, re)) {
          | None => args |> List.rev
          | Some(res) =>
            let res =
              res |> Js.Re.captures |> Array.map(Js.Nullable.toOption);
            switch (res) {
            | [|Some(m), _, _, Some(arg)|]
            | [|Some(m), Some(arg), _, _|]
            | [|Some(m), _, Some(arg), _|] =>
              let to_parse = sub_str(to_parse, m);
              p'([arg, ...args], to_parse);
            | _ => failwith("Wat?")
            };
          };
        };
        let args =
          p'(
            [],
            String.sub(
              cmd,
              first_space + 1,
              String.length(cmd) - first_space - 1,
            )
            ++ " ",
          );
        Some((String.sub(cmd, 0, first_space), args));
      };
    };
  };
};

module Term = {
  type parser('a) =
    (Info.eval, Runner.t) =>
    ('a, [ | `Parse(string) | `Error(bool, string) | `Help(string)]);

  type t('a) = (Info.args, parser('a));

  let const = v => (Info.Args.empty, (_, _) => Belt.Result.Ok(v));

  let eval = (~raw, (al, f), ti) => {
    let term = Info.term_add_args(ti, al);
    let ei = Info.eval(~term, ~main=term, ~choices=[]);
    let (cmd, args) = Runner.parse_str(raw) |> Belt.Option.getExn;
    ();
  };
};