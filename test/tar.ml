open Alcotest

let tar_test = [
  test_case "[tar] entry evaluation fails : Option as first token" `Quick 
    (Utils.test_evaluated_feature_id_fails "-c tar -v" Native_registry.Tar.tar);
  test_case "[tar] entry evaluation fails : No arguments provided" `Quick 
    (Utils.test_evaluated_feature_id_fails "tar -c -v" Native_registry.Tar.tar);
  test_case "[tar] entry evaluation succeeds : Arguments provided" `Quick 
    (Utils.test_evaluated_feature_id "create_archive" "tar -c -v file1.txt file2.txt" Native_registry.Tar.tar);
  test_case "[tar] entry evaluation succeeds : bad option location -> interpreted as an argument" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "create_archive" 
      "tar file1.txt -c" 
      Native_registry.Tar.tar
      { 
      Opencommand.Entry.options = Opencommand.StringMap.empty; 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "files" (
          Opencommand.Typeformat.VList [Opencommand.Typeformat.VString "file1.txt"; Opencommand.Typeformat.VString "-c"]
        ) 
      }
    );
]