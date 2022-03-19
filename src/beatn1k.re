open Cmdliner;

exception InvalidCommand(string);

let cmd = {
  let doc = "beatn1k is an application that creates weird dadaist and beat poems from URLs";

  let web = {
    let doc = "Web mode";
    Arg.(value & flag & info(["w", "web"], ~doc));
  };

  let mode = {
    let doc = "Mode can be cut (for cut-up) or fold (for fold-ups";
    Arg.(value & opt(string, "cut") & info(["m", "mode"], ~docv="MODE", ~doc))
  }

  let url_1 = {
    let doc = "First for cut-ups, source for left side of fold-ups";
    Arg.(value & opt(string, "") & info(["u1", "url_1"], ~docv="URL_1", ~doc))
  }

  let url_2 = {
    let doc = "Source for right side of fold-ups";
    Arg.(value & opt(string, "") & info(["u2", "url_2"], ~docv="URL_2", ~doc))
  }

  let cut_s = {
    let doc = "Size of word word cut for cut-ups";
    Arg.(value & opt(int, 4) & info(["c", "cuts"], ~docv="CUT_SIZE", ~doc))
  }

  let line_s = {
    let doc = "Size of each line";
    Arg.(value & opt(int, 10) & info(["l", "line"], ~docv="LINE_SIZE", ~doc))
  }

  let stanza_s = {
    let doc = "Size of stanza";
    Arg.(value & opt(int, 12) & info(["s", "stanza"], ~docv="STANZA_SIZE", ~doc))
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
};

let main () = exit (Cmd.eval(cmd))
let () = main ()