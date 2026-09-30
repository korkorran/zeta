
let successfull_feature_Ids entry (command:Opencommand.Command.command) = 
  let results = Command_req.Entry.evalConfiguration entry command.features in
  List.filter_map (fun (path, result:string * Command_req.Entry.result) ->
    Format.fprintf Format.std_formatter "Testing path: %s -> %a \n" path Command_req.Entry.pp_result result;
    match result with
    | Command_req.Entry.Success conf -> Some (path, conf)
    | _ -> None
  ) results 
let test_evaluated_feature_id feature entry cmd () =
  Alcotest.(check (list string)) "same path" [feature] (List.map fst (successfull_feature_Ids entry cmd))

let test_evaluated_feature_id_fails entry cmd () =
  Alcotest.(check (list string)) "same path" [] (List.map fst (successfull_feature_Ids entry cmd))

let equal_configuration (c1:Command_req.Entry.configuration) (c2:Command_req.Entry.configuration) =
  Opencommand.StringMap.equal Command_req.Typeformat.equal_value c1.options c2.options
  && Opencommand.StringMap.equal Command_req.Typeformat.equal_value c1.args c2.args

let test_evaluated_feature_id_with_configuration feature entry cmd expectedConfiguration () =
  let results = successfull_feature_Ids entry cmd in
  Alcotest.(check (list string)) "same path" [feature] (List.map fst results);
  match results with
  | [(_path, configuration)] -> Alcotest.(check bool) "same configuration" true (equal_configuration expectedConfiguration configuration)
  | _ -> Alcotest.failf "Expected one successful feature evaluation but got %d" (List.length results)


let generate_entry (command:Opencommand.Command.command) (feature:string) (configuration:Command_req.Entry.configuration) : (string, Command_req.Reverse.error) Result.t =
  Command_req.Reverse.computeEntry command feature configuration

let test_generated_entry expectedEntry command feature configuration () =
  let result = generate_entry command feature configuration in
  match result with
  | Result.Ok entry -> Alcotest.(check string) "same entry" expectedEntry entry
  | Result.Error err -> Alcotest.failf "Expected entry but got error: %a" Command_req.Reverse.pp_error err

let test_generated_entry_fails expectedError command feature configuration () =
  let result = generate_entry command feature configuration in
  match result with
  | Result.Ok entry -> Alcotest.failf "Expected error but got entry: %s" entry
  | Result.Error err -> Format.fprintf Format.std_formatter "Result: %a\n" Command_req.Reverse.pp_error err;
    Alcotest.(check bool) "same error" true (err = expectedError)