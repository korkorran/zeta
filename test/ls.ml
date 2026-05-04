open Alcotest

let ls_test = [
  test_case "[ls] entry evaluation" `Quick (Utils.test_entry "list_directory_contents" "ls -a" Native_registry.Ls.ls);
]