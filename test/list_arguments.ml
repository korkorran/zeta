open Alcotest
open Ocf
open Ocf.StringMap
open Zeta.Typeformat

(* a command taking between 2 and 3 integers *)
let sum : Command.command = {
  info = { commandName = "sum"; version = "UNVERSIONED"; description = "Sum integers" };
  features = empty
    |> Feature.addFeature "sum" {
      options = empty;
      anchors = empty;
      args = empty
        |> add "numbers" (Feature.Argument.Any { encoding = Feature.Argument.Between (2, 3, TokenEncoding.Integer); description = "Numbers to sum" });
      description = "Sum integers";
      path = FeaturePath.Token.[Directive "sum"; ArgumentLocation "numbers"];
    };
}

let list_arguments_test = [
  test_case "[list] entry evaluation fails : below minimum" `Quick 
    (Utils.test_evaluated_feature_id_fails "sum 1" sum);
  test_case "[list] entry evaluation succeeds : values are decoded with the argument encoding" `Quick 
    (Utils.test_evaluated_feature_id_with_configuration 
      "sum" 
      "sum 1 2" 
      sum
      { Zeta.Entry.options = empty; args = empty |> add "numbers" (Value (TList TInt, [1; 2])) }
    );
  test_case "[list] entry evaluation succeeds : maximum reached" `Quick 
    (Utils.test_evaluated_feature_id "sum" "sum 1 2 3" sum);
  test_case "[list] entry evaluation fails : above maximum" `Quick 
    (Utils.test_evaluated_feature_id_fails "sum 1 2 3 4" sum);
  test_case "[list] entry evaluation fails : value not matching the encoding" `Quick 
    (Utils.test_evaluated_feature_id_fails "sum 1 two" sum);
]
