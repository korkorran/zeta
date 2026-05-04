(* A module with functions to test *)

let test_tokenizer s tokens () =
  let lexbuf = Lexing.from_string s in
  let result = Entry_tokenizer.Main.parse_with_error lexbuf
  in
  Alcotest.(check (option (list string))) "tokens" tokens result

(* Run it *)
let () =
  let open Alcotest in
  run "OpenCommand" [
      "git", Git.git_test;
      "tokenizer", [
        test_case "tokenize 2 words" `Quick (test_tokenizer "name Alice" (Some ["name"; "Alice"]));
        test_case "tokenize 3 words" `Quick (test_tokenizer "name Alice \"hello\"" (Some ["name"; "Alice"; "\"hello\""]));
        test_case "tokenize 4 words" `Quick (test_tokenizer "git commit -m \"a commit message\"" 
          (Some ["git"; "commit"; "-m"; "\"a commit message\""]));
        test_case "mv cmd" `Quick (test_tokenizer "mv file1.txt file2.txt" (Some ["mv"; "file1.txt"; "file2.txt"]));
        test_case "tokenize fails" `Quick (test_tokenizer "name Alice \"hello" (None));
      ];
      "ls", [
          test_case "[ls] entry evaluation" `Quick (Utils.test_entry "ls [?options] [<directories>]" "ls -a" Native_registry.Ls.ls);
        ];
    ]