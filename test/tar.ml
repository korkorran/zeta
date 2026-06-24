open Alcotest

let tar_test = [
  test_case "[tar] entry evaluation fails : No arguments provided" `Quick 
    (Utils.test_evaluated_feature_id_fails "tar -c -v" Native_registry.Tar.tar);
  test_case "[tar] entry evaluation succeeds : Arguments provided" `Quick 
    (Utils.test_evaluated_feature_id "create_archive" "tar -c -v file1.txt file2.txt" Native_registry.Tar.tar);
(*   test_case "[tar] entry evaluation fails : bad option location" `Quick 
    (Utils.test_evaluated_feature_id_fails "tar file1.txt -c" Native_registry.Tar.tar); *)
]