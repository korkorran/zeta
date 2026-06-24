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
    ]