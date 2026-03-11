(* A module with functions to test *)
let git = Native_registry.Git.git
(* The tests *)
let test_description () =
  Alcotest.(check string) "same string" "Git version control system" (Opencommand.Utils.StringMap.find "git" git).description

let successfull_paths entry command = 
  let results = Opencommand.Entry.evalConfiguration entry command in
  List.filter_map (fun (path, result:string * Opencommand.Entry.result) ->
    Format.fprintf Format.std_formatter "Testing path: %s -> %a \n" path Opencommand.Entry.pp_result result;
    match result with
    | Opencommand.Entry.Success _ -> Some path
    | _ -> None
  ) results 
let test_entry () =
  Alcotest.(check (list string)) "same path" ["git config [?options]"] (successfull_paths "git config --list" git)

let test_tokenizer s tokens () =
  let lexbuf = Lexing.from_string s in
  let result = Entry_tokenizer.Main.parse_with_error lexbuf
  in
  Alcotest.(check (option (list string))) "tokens" tokens result

(* Run it *)
let () =
  let open Alcotest in
  run "OpenCommand" [
      "git", [
          test_case "description match" `Quick test_description;
          test_case "entry evaluation" `Quick test_entry
        ];
      "tokenizer", [
        test_case "tokenize 2 words" `Quick (test_tokenizer "name Alice" (Some ["name"; "Alice"]));
        test_case "tokenize 3 words" `Quick (test_tokenizer "name Alice \"hello\"" (Some ["name"; "Alice"; "\"hello\""]));
        test_case "tokenize 4 words" `Quick (test_tokenizer "git commit -m \"a commit message\"" 
          (Some ["git"; "commit"; "-m"; "\"a commit message\""]));
        test_case "mv cmd" `Quick (test_tokenizer "mv file1.txt file2.txt" (Some ["mv"; "file1.txt"; "file2.txt"]));
        test_case "tokenize fails" `Quick (test_tokenizer "name Alice \"hello" (None));
      ]
    ]