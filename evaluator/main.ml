open Ocf
open Cmdliner

let registry = Registry.Index.registry

let path_to_string path =
  String.concat " " (List.map FeaturePath.Token.token_to_string path)

(* pads [s] to [width] characters, counting UTF-8 characters rather than bytes (e.g. the anchor marker §) *)
let pad width s =
  let length = String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s in
  s ^ String.make (max 0 (width - length)) ' '

(* browse *)

let print_command (command : Command.command) =
  Printf.printf "%s (%s) : %s\n" command.info.commandName command.info.version command.info.description;
  StringMap.iter (fun id (feature : Feature.feature) ->
    Printf.printf "  %-24s %s %s\n" id (pad 40 (path_to_string feature.path)) feature.description
  ) command.features

let browse () =
  StringMap.iter (fun _ command -> print_command command; print_newline ()) registry

(* eval *)

let print_values title values =
  if not (StringMap.is_empty values) then begin
    Printf.printf "    %s:\n" title;
    StringMap.iter (fun name value ->
      Format.printf "      %s = %a@." name Repr.Typeformat.pp_value value
    ) values
  end

let print_result (feature, result) =
  match result with
  | Zeta.EvalFeature.Success (configuration : Repr.Configuration.configuration) ->
    Printf.printf "  [ok]   %s\n" feature;
    print_values "options" configuration.options;
    print_values "args" configuration.args
  | Zeta.EvalFeature.Unmatched token ->
    Printf.printf "  [fail] %s : the entry does not match '%s'\n" feature (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Incomplete token ->
    Printf.printf "  [fail] %s : the entry ends before '%s'\n" feature (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Overflow token ->
    Printf.printf "  [fail] %s : unexpected '%s'\n" feature token

let eval_entry words =
  let entry = String.concat " " words in
  match words with
  | [] -> `Error (true, "an entry is required")
  | name :: _ ->
    match StringMap.find_opt (List.hd (String.split_on_char ' ' name)) registry with
    | None ->
      let known = String.concat ", " (List.map fst (StringMap.bindings registry)) in
      `Error (false, Printf.sprintf "unknown command '%s' (known commands: %s)" name known)
    | Some command ->
      let results = Zeta.Entry.evalEntry entry command in
      Printf.printf "%s\n" entry;
      List.iter print_result results;
      let success = List.exists (function (_, Zeta.EvalFeature.Success _) -> true | _ -> false) results in
      `Ok (if success then 0 else 1)

(* command line *)

let browse_cmd =
  let doc = "List the commands described in the registry, with their features." in
  Cmd.v (Cmd.info "browse" ~doc) Term.(const (fun () -> browse (); 0) $ const ())

let eval_cmd =
  let doc = "Evaluate a command line entry against the registry with the zeta algorithm." in
  let man = [
    `S Manpage.s_description;
    `P "The command is selected by the first word of the entry. Quote the whole entry, \
        or put it after $(b,--) when it contains options:";
    `Pre "  main.exe eval 'git commit -m \"Initial commit\"'\n  main.exe eval -- mv -v a.txt dir";
  ] in
  let entry = Arg.(value & pos_all string [] & info [] ~docv:"ENTRY" ~doc:"The command line entry to evaluate.") in
  Cmd.v (Cmd.info "eval" ~doc ~man) Term.(ret (const eval_entry $ entry))

let () =
  let doc = "Browse the registry and evaluate command line entries." in
  exit (Cmd.eval' (Cmd.group (Cmd.info "evaluator" ~doc) [browse_cmd; eval_cmd]))
