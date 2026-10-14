open Ocf

let registry = Registry.Index.registry

(* the path of the feature [id] of [command] *)
let feature_path (command : Command.command) id =
  Browse.path_to_string (StringMap.find id command.features).path

let print_values title values =
  if not (StringMap.is_empty values) then begin
    Printf.printf "    %s:\n" title;
    StringMap.iter (fun name value ->
      Format.printf "      %s = %a@." name Repr.Typeformat.pp_value value
    ) values
  end

let print_result (command : Command.command) (feature, result) =
  match result with
  | Zeta.EvalFeature.Success (configuration : Repr.Configuration.configuration) ->
    Printf.printf "  [ok]   %s : %s\n" feature (feature_path command feature);
    print_values "options" configuration.options;
    print_values "args" configuration.args
  | Zeta.EvalFeature.Unmatched token ->
    Printf.printf "  [fail] %s : the entry does not match '%s'\n" feature (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Incomplete token ->
    Printf.printf "  [fail] %s : the entry ends before '%s'\n" feature (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Overflow token ->
    Printf.printf "  [fail] %s : unexpected '%s'\n" feature token

let find_command entry =
  match String.split_on_char ' ' (String.trim entry) with
  | name :: _ -> StringMap.find_opt name registry
  | [] -> None

let is_success = function (_, Zeta.EvalFeature.Success _) -> true | _ -> false

(* non interactive evaluation, when the output is not a terminal *)
let print_evaluation entry =
  match find_command entry with
  | None ->
    let known = String.concat ", " (List.map fst (StringMap.bindings registry)) in
    `Error (false, Printf.sprintf "unknown command in '%s' (known commands: %s)" entry known)
  | Some command ->
    let results = Zeta.Entry.evalEntry entry command in
    Printf.printf "%s\n" entry;
    List.iter (print_result command) results;
    `Ok (if List.exists is_success results then 0 else 1)

(* interactive evaluation *)

let failure_to_string = function
  | Zeta.EvalFeature.Success _ -> "matches"
  | Zeta.EvalFeature.Unmatched token -> Printf.sprintf "the entry does not match '%s'" (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Incomplete token -> Printf.sprintf "the entry ends before '%s'" (FeaturePath.Token.token_to_string token)
  | Zeta.EvalFeature.Overflow token -> Printf.sprintf "unexpected '%s'" token

(* the lines (as styled text markup) describing the evaluation of an entry *)
let evaluation_markup entry : LTerm_text.markup =
  let ok text = [LTerm_text.B_fg LTerm_style.green; S ("✔ " ^ text); E_fg; S "\n"] in
  let ko text = [LTerm_text.B_fg LTerm_style.red; S ("✘ " ^ text); E_fg; S "\n"] in
  let dim text = [LTerm_text.B_fg LTerm_style.blue; S text; E_fg; S "\n"] in
  let values title values =
    if StringMap.is_empty values then []
    else [LTerm_text.S ("    " ^ title ^ "\n")]
      @ List.concat_map (fun (name, (Repr.Typeformat.Value (tf, _) as value)) ->
          [LTerm_text.S (Format.asprintf "      %s : %s = %a\n" name (Browse.typeformat_to_string tf) Repr.Typeformat.pp_value value)]
        ) (StringMap.bindings values)
  in
  match find_command entry with
  | None ->
    let known = String.concat ", " (List.map fst (StringMap.bindings registry)) in
    ko "no command of the registry matches the entry" @ dim ("  known commands: " ^ known)
  | Some command ->
    let results = Zeta.Entry.evalEntry entry command in
    let command_line = ok (Printf.sprintf "command  %s : %s" command.info.commandName command.info.description) in
    match List.filter is_success results with
    | [] ->
      command_line
      @ ko "no feature matches the entry"
      @ List.concat_map (fun (feature, result) -> dim (Printf.sprintf "  %s : %s" feature (failure_to_string result))) results
    | successes ->
      command_line
      @ List.concat_map (fun (feature, result) ->
          match result with
          | Zeta.EvalFeature.Success (configuration : Repr.Configuration.configuration) ->
            ok (Printf.sprintf "feature  %s : %s" feature (feature_path command feature))
            @ (if StringMap.is_empty configuration.options && StringMap.is_empty configuration.args
               then dim "    (empty configuration)"
               else values "options" configuration.options @ values "args" configuration.args)
          | _ -> []
        ) successes

(* a line editor prefilled with [initial] *)
class read_entry term history initial = object (self)
  inherit LTerm_read_line.read_line ~history:(LTerm_history.contents history) ()
  inherit [Zed_string.t] LTerm_read_line.term term

  (* no completion box under the edition line *)
  method! show_box = false

  initializer
    self#set_prompt (React.S.const (LTerm_text.of_utf8 "entry> "));
    Zed_edit.insert self#context (Zed_rope.of_string (Zed_string.of_utf8 initial))
end

let interactive_eval initial =
  let open Lwt.Syntax in
  let* term = Lazy.force LTerm.stdout in
  let history = LTerm_history.create [] in
  let* () = LTerm.fprintls term (LTerm_text.eval [B_fg LTerm_style.blue; S "Enter: evaluate the entry   Ctrl-C: quit"; E_fg]) in
  let rec loop entry =
    let* line =
      Lwt.catch
        (fun () -> let+ line = (new read_entry term history entry)#run in Some (Zed_string.to_utf8 line))
        (function LTerm_read_line.Interrupt | Sys.Break -> Lwt.return_none | e -> Lwt.fail e)
    in
    match line with
    | None -> Lwt.return_unit
    | Some entry ->
      LTerm_history.add history (Zed_string.of_utf8 entry);
      let* () = LTerm.fprints term (LTerm_text.eval (evaluation_markup entry)) in
      let* () = LTerm.fprintl term "" in
      loop entry
  in
  loop initial

(* evaluates the entry made of [words]: interactively in a terminal, once otherwise *)
let run words =
  let entry = String.concat " " words in
  if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then begin
    Lwt_main.run (interactive_eval entry);
    `Ok 0
  end
  else if words = [] then `Error (true, "an entry is required")
  else print_evaluation entry
