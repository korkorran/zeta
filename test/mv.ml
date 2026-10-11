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
      "mv -v file1.txt file2.txt" 
      Native_registry.Mv.mv
      { 
      Zeta.Entry.options = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "verbose" Zeta.Typeformat.(Value (TOption TUnit, Some ())); 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "source" (
          Zeta.Typeformat.(Value (TList (TString None), ["file1.txt"]))
        ) |>
        Opencommand.StringMap.add "destination" Zeta.Typeformat.(Value (TString None, "file2.txt"))
      }
    );
  test_case "[mv] entry evaluation succeeds with configuration - bad option location" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv file1.txt -v file2.txt" 
      Native_registry.Mv.mv
      { 
      Zeta.Entry.options = Opencommand.StringMap.empty; 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "source" (
          Zeta.Typeformat.(Value (TList (TString None), ["file1.txt"; "-v"]))
        ) |>
        Opencommand.StringMap.add "destination" Zeta.Typeformat.(Value (TString None, "file2.txt"))
      }
    );
  test_case "[mv] entry evaluation succeeds with configuration - many sources" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv a.txt b.txt c.txt d.txt dir" 
      Native_registry.Mv.mv
      { 
      Zeta.Entry.options = Opencommand.StringMap.empty; 
      args = Opencommand.StringMap.empty |>
        Opencommand.StringMap.add "source" (
          Zeta.Typeformat.(Value (TList (TString None), ["a.txt"; "b.txt"; "c.txt"; "d.txt"]))
        ) |>
        Opencommand.StringMap.add "destination" Zeta.Typeformat.(Value (TString None, "dir"))
      }
    );
]
