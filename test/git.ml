open Alcotest
open Opencommand.Utils.StringMap

let git = Native_registry.Git.git
(* The tests *)
let test_description () =
  (check string) "same string" "Git version control system" (find "git" git.paths).description
let git_test = [
  test_case "description match" `Quick test_description;
  test_case "[get_config] entry evaluation" `Quick (Utils.test_evaluated_feature_id "get_config" "git config --list" git);
  test_case "[get_config] generated entry #1" `Quick (Utils.test_generated_entry 
    "git config" 
    git 
    "get_config" 
    { Opencommand.Entry.options = empty; args = empty }
  );
  test_case "[get_config] generated entry #2" `Quick (Utils.test_generated_entry 
    "git config --list" 
    git 
    "get_config" 
    { Opencommand.Entry.options = empty |> add "--list" (Opencommand.Typeformat.VUnit); args = empty }
  );
  test_case "[create_branch] generated entry #1" `Quick (Utils.test_generated_entry 
    "git branch my_branch" 
    git 
    "create_branch" 
    { Opencommand.Entry.options = empty; args = empty |> add "branch_name" (Opencommand.Typeformat.VString "my_branch") }
  );
]