open Alcotest

let ls_test = [
  test_case "[ls] entry evaluation" `Quick (Utils.test_evaluated_feature_id "list_directory_contents" "ls -a" Native_registry.Ls.ls);
]