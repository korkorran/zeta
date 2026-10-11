open Ocf

let dotFeatures = StringMap.empty
  |> Feature.addFeature "generate_visualization" {
    options = StringMap.empty
      |> StringMap.add "output" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.ValueSuffix (["-o"], TokenEncoding.SimpleString (Some TokenEncoding.FilePath)); description = "write output in file"; location = "options" });
    anchors = StringMap.empty
      |> StringMap.add "options" [];
    args = StringMap.empty
      |> StringMap.add "input" (Feature.Argument.Any { encoding = Feature.Argument.Optional (TokenEncoding.SimpleString (Some TokenEncoding.FilePath)); description = "Input file/directory" });
    description = "Compute graphviz visualization";
    path = FeaturePath.Token.[Directive "dot"; Anchor "options"; ArgumentLocation "input"];
  }

let dot : Ocf.Command.command = {
  info = {
    commandName = "dot";
    version = "UNVERSIONED";
    description = "Compute graphviz visualization";
  };
  features = dotFeatures;
}
