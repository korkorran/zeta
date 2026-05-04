
let successfull_paths entry (command:Opencommand.Command.command) = 
  let results = Opencommand.Entry.evalConfiguration entry command.paths in
  List.filter_map (fun (path, result:string * Opencommand.Entry.result) ->
    Format.fprintf Format.std_formatter "Testing path: %s -> %a \n" path Opencommand.Entry.pp_result result;
    match result with
    | Opencommand.Entry.Success _ -> Some path
    | _ -> None
  ) results 
let test_entry path entry cmd () =
  Alcotest.(check (list string)) "same path" [path] (successfull_paths entry cmd)