(* A module with functions to test *)
let git = Native_registry.Git.git
(* The tests *)
let test_description () =
  Alcotest.(check string) "same string" "Git version control system" (Mapping.Command.StringMap.find "git" git).description

let successfull_paths entry command = 
  let results = Mapping.Entry.evalConfiguration entry command in
  List.filter_map (fun (path, result:string * Mapping.Entry.result) ->
    Format.fprintf Format.std_formatter "Testing path: %s -> %a \n" path Mapping.Entry.pp_result result;
    match result with
    | Mapping.Entry.Success _ -> Some path
    | _ -> None
  ) results 
let test_entry () =
  Alcotest.(check (list string)) "same path" ["git config [?options]"] (successfull_paths "git config --list" git)

let test_tokenizer () =
  let _result = Entry_tokenizer.Main.loop 
    "name Alice" 
  in
  Alcotest.(check (bool)) "tokens" true (true)

(* Run it *)
let () =
  let open Alcotest in
  run "OpenCommand" [
      "git", [
          test_case "description match" `Quick test_description;
          test_case "entry evaluation" `Quick test_entry
        ];
      "tokenizer", [
        test_case "tokenize json" `Quick test_tokenizer
      ]
    ]