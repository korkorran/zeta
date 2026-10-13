(* A module with functions to test *)


(* Run it *)
let () =
  let open Alcotest in
  run "OpenCommand" [
      "entry_tokenizer", Entry_tokenizer_test.tokenizer_test;
      "git", Git.git_test;
      "ls", Ls.ls_test;
      "tar", Tar.tar_test;
      "mv", Mv.mv_test;
      "dot", Dot.dot_test;
      "cd", Cd.cd_test;
      "list_arguments", List_arguments.list_arguments_test;
    ]