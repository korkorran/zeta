open Alcotest
open Opencommand.StringMap

let git = Native_registry.Git.git
(* The tests *)
let test_description () =
  (check string) "same string" "Git version control system" (find "git_usage" git.features).description
let git_test = [
  test_case "description match" `Quick test_description;
  test_case "[get_config] entry evaluation succeeds #1" `Quick (Utils.test_evaluated_feature_id "get_config" "git config --list" git);
  test_case "[commit] entry evaluation succeeds #2" `Quick (Utils.test_evaluated_feature_id "commit" "git commit -m \"Initial commit\"" git);
  test_case "[commit] entry evaluation fails : bad option location" `Quick (Utils.test_evaluated_feature_id_fails "git -m \"Initial commit\" commit" git);
  test_case "[get_config] generated entry #1" `Quick (Utils.test_generated_entry 
    "git config" 
    git 
    "get_config" 
    { Command_req.Entry.options = empty; args = empty }
  );
  test_case "[get_config] generated entry #2" `Quick (Utils.test_generated_entry 
    "git config --list" 
    git 
    "get_config" 
    { Command_req.Entry.options = empty |> add "--list" Command_req.Typeformat.(Value (TOption TUnit, Some ())); args = empty }
  );
  test_case "[create_branch] generated entry #3" `Quick (Utils.test_generated_entry 
    "git branch my_branch" 
    git 
    "create_branch" 
    { Command_req.Entry.options = empty; args = empty |> add "branch_name" Command_req.Typeformat.(Value (TString None, "my_branch")) }
  );
  test_case "[create_branch] generated entry #4 (fails)" `Quick (Utils.test_generated_entry_fails 
    (Command_req.Reverse.ValidationError [(Command_req.Configuration.Arg , "branch_name", Command_req.Configuration.Format (Command_req.Typeformat.FormatError (Command_req.Typeformat.Any (Command_req.Typeformat.TString (Some Opencommand.TokenEncoding.SingleWord)))))])
    git 
    "create_branch" 
    { Command_req.Entry.options = empty; args = empty |> add "branch_name" Command_req.Typeformat.(Value (TString None, "my branch")) }
  );
]