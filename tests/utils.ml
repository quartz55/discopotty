let setup_logging ?(level = Relog.Level.Debug) () =
  let cli_fmter = Relog.Formatter.default ~color:true ~oneline:false () in
  let cli_fmt = Format.formatter_of_out_channel stderr in
  Relog.Sink.make_mutex (fun r ->
      if Relog.(Level.compare (Record.level r) level) <= 0 then
        cli_fmter cli_fmt r
      else ())
  |> Relog.Sink.set
