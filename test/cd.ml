open Alcotest

let cd_test = [
  test_case "[cd] entry evaluation succeeds with configuration : file path" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "change_directory" 
      "cd my/dir" 
      Registry.Cd.cd
      { 
      Repr.Configuration.options = Ocf.StringMap.empty; 
      args = Ocf.StringMap.empty |>
        Ocf.StringMap.add "directory" Repr.Typeformat.(Value (TString None, "my/dir"))
      }
    );
  test_case "[cd] entry evaluation fails : not a file path" `Quick 
    (Utils.test_evaluated_feature_id_fails "cd \"a b\"" Registry.Cd.cd);
]
