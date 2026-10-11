open Alcotest
open Opencommand.StringMap
open Zeta.Typeformat

let dot = Native_registry.Dot.dot

let dot_test = [
  test_case "[dot] entry evaluation succeeds : no input" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "generate_visualization" 
      "dot" 
      dot
      { Zeta.Entry.options = empty; args = empty |> add "input" (Value (TOption (TString None), None)) }
    );
  test_case "[dot] entry evaluation succeeds : input provided" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "generate_visualization" 
      "dot graph.gv" 
      dot
      { Zeta.Entry.options = empty; args = empty |> add "input" (Value (TOption (TString None), Some "graph.gv")) }
    );
  test_case "[dot] entry evaluation succeeds : option and input provided" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "generate_visualization" 
      "dot -o out.png graph.gv" 
      dot
      { 
      Zeta.Entry.options = empty |> add "output" (Value (TString None, "out.png")); 
      args = empty |> add "input" (Value (TOption (TString None), Some "graph.gv"))
      }
    );
  test_case "[dot] entry evaluation fails : too many inputs" `Quick 
    (Utils.test_evaluated_feature_id_fails "dot a.gv b.gv" dot);
  test_case "[dot] generated entry : no input" `Quick (Utils.test_generated_entry 
    "dot" 
    dot 
    "generate_visualization" 
    { Zeta.Entry.options = empty; args = empty |> add "input" (Value (TOption (TString None), None)) }
  );
  test_case "[dot] generated entry : input provided" `Quick (Utils.test_generated_entry 
    "dot -o out.png graph.gv" 
    dot 
    "generate_visualization" 
    { 
    Zeta.Entry.options = empty |> add "output" (Value (TString None, "out.png")); 
    args = empty |> add "input" (Value (TOption (TString None), Some "graph.gv"))
    }
  );
]
