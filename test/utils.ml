
let successfull_feature_Ids entry (command:Ocf.Command.command) = 
  let results = Zeta.Entry.evalConfiguration entry command.features in
  List.filter_map (fun (path, result:string * Zeta.Entry.result) ->
    Format.fprintf Format.std_formatter "Testing path: %s -> %a \n" path Zeta.Entry.pp_result result;
    match result with
    | Zeta.Entry.Success conf -> Some (path, conf)
    | _ -> None
  ) results 
let test_evaluated_feature_id feature entry cmd () =
  Alcotest.(check (list string)) "same path" [feature] (List.map fst (successfull_feature_Ids entry cmd))

let test_evaluated_feature_id_fails entry cmd () =
  Alcotest.(check (list string)) "same path" [] (List.map fst (successfull_feature_Ids entry cmd))

let equal_configuration (c1:Zeta.Entry.configuration) (c2:Zeta.Entry.configuration) =
  Ocf.StringMap.equal Zeta.Typeformat.equal_value c1.options c2.options
  && Ocf.StringMap.equal Zeta.Typeformat.equal_value c1.args c2.args

let test_evaluated_feature_id_with_configuration feature entry cmd expectedConfiguration () =
  let results = successfull_feature_Ids entry cmd in
  Alcotest.(check (list string)) "same path" [feature] (List.map fst results);
  match results with
  | [(_path, configuration)] -> Alcotest.(check bool) "same configuration" true (equal_configuration expectedConfiguration configuration)
  | _ -> Alcotest.failf "Expected one successful feature evaluation but got %d" (List.length results)


let generate_entry (command:Ocf.Command.command) (feature:string) (configuration:Zeta.Entry.configuration) : (string, Zeta.Reverse.error) Result.t =
  Zeta.Reverse.computeEntry command feature configuration

let test_generated_entry expectedEntry command feature configuration () =
  let result = generate_entry command feature configuration in
  match result with
  | Result.Ok entry -> Alcotest.(check string) "same entry" expectedEntry entry
  | Result.Error err -> Alcotest.failf "Expected entry but got error: %a" Zeta.Reverse.pp_error err

let test_generated_entry_fails expectedError command feature configuration () =
  let result = generate_entry command feature configuration in
  match result with
  | Result.Ok entry -> Alcotest.failf "Expected error but got entry: %s" entry
  | Result.Error err -> Format.fprintf Format.std_formatter "Result: %a\n" Zeta.Reverse.pp_error err;
    Alcotest.(check bool) "same error" true (err = expectedError)