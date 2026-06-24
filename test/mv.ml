open Alcotest

let mv_test = [
  test_case "[mv] entry evaluation fails : Option as first token" `Quick 
    (Utils.test_evaluated_feature_id_fails "-f mv -v" Native_registry.Mv.mv);
  test_case "[mv] entry evaluation fails : No arguments provided" `Quick 
    (Utils.test_evaluated_feature_id_fails "mv -f -v README.md" Native_registry.Mv.mv);
  test_case "[mv] entry evaluation succeeds : Arguments provided" `Quick 
    (Utils.test_evaluated_feature_id "move_or_rename" "mv file1.txt file2.txt" Native_registry.Mv.mv);
  test_case "[mv] entry evaluation succeeds with configuration" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv file1.txt -v file2.txt" 
      Native_registry.Mv.mv
      { 
      Opencommand.Entry.options = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "verbose" (Opencommand.Typeformat.VUnit); 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "source" (
          Opencommand.Typeformat.VList [Opencommand.Typeformat.VString "file1.txt"]
        ) |>
        Opencommand.StringMap.add "destination" (Opencommand.Typeformat.VString "file2.txt")
      }
    );
]