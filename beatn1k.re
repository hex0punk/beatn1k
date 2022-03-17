open Cmdliner;

exception InvalidCommand(string);

let cmd = {
  let doc = "Simple CLI built in Reason";

  let web = {
    let doc = "Web mode";
    Arg.(value & flag & info(["w", "web"], ~doc));
  };

  let mode = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(string, "cut") & info(["m", "mode"], ~docv="MODE", ~doc))
  }

  let url_1 = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(string, "") & info(["u1", "url_1"], ~docv="URL_1", ~doc))
  }

  let url_2 = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(string, "") & info(["u2", "url_2"], ~docv="URL_2", ~doc))
  }

  let cut_s = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(int, 4) & info(["c", "cuts"], ~docv="LINE", ~doc))
  }

  let line_s = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(int, 10) & info(["l", "line"], ~docv="LINE", ~doc))
  }

  let stanza_s = {
    let doc = "Repeat the message $(docv) times.";
    Arg.(value & opt(int, 12) & info(["s", "stanza"], ~docv="STANZA", ~doc))
  }

  let run = (web, mode, url_1, url_2, cut_s, line_s, stanza_s) => {
    if (mode == "fold"){
      Dadait.run_foldup(~web=web, url_1, url_2, line_s, stanza_s)
    } else {
      Dadait.run_cutup(~web=web, url_1, cut_s, line_s, stanza_s)
    }
  };

  let info = Cmd.info("beatn1k", ~doc)
  let y = Term.(const(run) $ web $ mode $ url_1 $ url_2 $ cut_s $ line_s $ stanza_s);
  Cmd.v(info, y)
  // Cmd.v(info, Term.(const(run) $ web $ mode $ url_1 $ url_2 $ cut_s $ line_s $ stanza_s, info("beatn1k", ~doc)));
};

let main () = exit (Cmd.eval(cmd))
let () = main ()

// let () = Term.exit @@ Term.eval(cmd);