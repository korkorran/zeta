open Alcotest

let git = Native_registry.Git.git
(* The tests *)
let test_description () =
  (check string) "same string" "Git version control system" (Opencommand.Utils.StringMap.find "git" git.paths).description
let git_test = [
  test_case "description match" `Quick test_description;
  test_case "[git] entry evaluation" `Quick (Utils.test_evaluated_feature_id "get_config" "git config --list" git);
]