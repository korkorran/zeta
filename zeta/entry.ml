open Ocf

let evalEntry (rawEntry:string) (command:Command.command) : (string * EvalFeature.result) list =
  (* let command = Command.parseEncodedCommand rawCommand in *)
  let tokens_opt = Entry_tokenizer.Main.parse_with_error (Lexing.from_string rawEntry) in
  match tokens_opt with
  | Some entry ->
  (* Placeholder implementation *)
  List.map (fun (feature, head) ->
    let res = EvalFeature.evalFeature entry head in
    (feature, res)
  ) (StringMap.bindings command.features)
  | None -> []