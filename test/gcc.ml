open Alcotest
open Ocf.StringMap
open Repr.Typeformat

let gcc = Registry.Gcc.gcc

let configuration = {
  Repr.Configuration.options = empty
    |> add "include_dir" (Value (TOption (TString None), Some "include"))
    |> add "library_dir" (Value (TOption (TString None), Some "lib"))
    |> add "library" (Value (TOption (TString None), Some "maLib"))
    |> add "output" (Value (TOption (TString None), Some "monprog"));
  args = empty |> add "sources" (Value (TList (TString None), ["src/main.c"]))
}

let gcc_test = [
  test_case "[gcc] entry evaluation succeeds with configuration : prefixed options" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "compile" 
      "gcc -Iinclude src/main.c -Llib -lmaLib -o monprog" 
      gcc
      configuration
    );
  test_case "[gcc] entry evaluation : a prefix without value is not an option" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "compile" 
      "gcc -l src/main.c" 
      gcc
      { Repr.Configuration.options = empty; args = empty |> add "sources" (Value (TList (TString None), ["-l"; "src/main.c"])) }
    );
  test_case "[gcc] generated entry : prefixed options" `Quick (Utils.test_generated_entry 
    "gcc -Iinclude -lmaLib -Llib -o monprog src/main.c" 
    gcc 
    "compile" 
    configuration
  );
]
