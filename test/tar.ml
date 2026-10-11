open Alcotest

let tar_test = [
  test_case "[tar] entry evaluation fails : Option as first token" `Quick 
    (Utils.test_evaluated_feature_id_fails "-c tar -v" Registry.Tar.tar);
  test_case "[tar] entry evaluation fails : No arguments provided" `Quick 
    (Utils.test_evaluated_feature_id_fails "tar -c -v" Registry.Tar.tar);
  test_case "[tar] entry evaluation succeeds : Arguments provided" `Quick 
    (Utils.test_evaluated_feature_id "create_archive" "tar -c -v file1.txt file2.txt" Registry.Tar.tar);
  test_case "[tar] entry evaluation succeeds : bad option location -> interpreted as an argument" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "create_archive" 
      "tar file1.txt -c" 
      Registry.Tar.tar
      { 
      Zeta.Entry.options = Opencommand.StringMap.empty; 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "files" (
          Zeta.Typeformat.(Value (TList (TString None), ["file1.txt"; "-c"]))
        ) 
      }
    );
]