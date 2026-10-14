open Cmdliner

(* command line *)

let browse_cmd =
  let doc = "Browse interactively the commands of the registry, their features and their arguments, anchors and options." in
  Cmd.v (Cmd.info "browse" ~doc) Term.(const (fun () -> Browse.run (); 0) $ const ())

let eval_cmd =
  let doc = "Evaluate interactively a command line entry against the registry with the zeta algorithm." in
  let man = [
    `S Manpage.s_description;
    `P "Opens a line editor prefilled with $(i,ENTRY). Press Enter to evaluate the entry: \
        the matching command, the matching features and their configurations are displayed below, \
        and the entry can then be edited again. Press Ctrl-C (or Ctrl-D on an empty line) to quit. \
        When the output is not a terminal, the entry is evaluated once and the results are printed.";
    `P "The command is selected by the first word of the entry. Quote the whole entry, \
        or put it after $(b,--) when it contains options:";
    `Pre "  main.exe eval 'git commit -m \"Initial commit\"'\n  main.exe eval -- mv -v a.txt dir";
  ] in
  let entry = Arg.(value & pos_all string [] & info [] ~docv:"ENTRY" ~doc:"The command line entry to evaluate.") in
  Cmd.v (Cmd.info "eval" ~doc ~man) Term.(ret (const Eval.run $ entry))

let () =
  let doc = "Browse the registry and evaluate command line entries." in
  exit (Cmd.eval' (Cmd.group (Cmd.info "evaluator" ~doc) [browse_cmd; eval_cmd]))
