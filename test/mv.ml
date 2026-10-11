open Alcotest

let mv_test = [
  test_case "[mv] entry evaluation fails : Option as first token" `Quick 
    (Utils.test_evaluated_feature_id_fails "-f mv -v" Registry.Mv.mv);
  test_case "[mv] entry evaluation fails : No arguments provided" `Quick 
    (Utils.test_evaluated_feature_id_fails "mv -f -v README.md" Registry.Mv.mv);
  test_case "[mv] entry evaluation succeeds : Arguments provided" `Quick 
    (Utils.test_evaluated_feature_id "move_or_rename" "mv file1.txt file2.txt" Registry.Mv.mv);
  test_case "[mv] entry evaluation succeeds with configuration" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv -v file1.txt file2.txt" 
      Registry.Mv.mv
      { 
      Repr.Configuration.options = Ocf.StringMap.empty |>
        Ocf.StringMap.add "verbose" Repr.Typeformat.(Value (TOption TUnit, Some ())); 
      args = Ocf.StringMap.empty |>
        Ocf.StringMap.add "source" (
          Repr.Typeformat.(Value (TList (TString None), ["file1.txt"]))
        ) |>
        Ocf.StringMap.add "destination" Repr.Typeformat.(Value (TString None, "file2.txt"))
      }
    );
  test_case "[mv] entry evaluation succeeds with configuration - bad option location" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv file1.txt -v file2.txt" 
      Registry.Mv.mv
      { 
      Repr.Configuration.options = Ocf.StringMap.empty; 
      args = Ocf.StringMap.empty |>
        Ocf.StringMap.add "source" (
          Repr.Typeformat.(Value (TList (TString None), ["file1.txt"; "-v"]))
        ) |>
        Ocf.StringMap.add "destination" Repr.Typeformat.(Value (TString None, "file2.txt"))
      }
    );
  test_case "[mv] entry evaluation succeeds with configuration - many sources" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "move_or_rename" 
      "mv a.txt b.txt c.txt d.txt dir" 
      Registry.Mv.mv
      { 
      Repr.Configuration.options = Ocf.StringMap.empty; 
      args = Ocf.StringMap.empty |>
        Ocf.StringMap.add "source" (
          Repr.Typeformat.(Value (TList (TString None), ["a.txt"; "b.txt"; "c.txt"; "d.txt"]))
        ) |>
        Ocf.StringMap.add "destination" Repr.Typeformat.(Value (TString None, "dir"))
      }
    );
]
