open Opencommand

let dotFeatures = StringMap.empty
  |> Feature.addFeature "generate_visualization" {
    options = StringMap.empty
      |> StringMap.add "output" { Feature.CommandOption.encoding = Feature.CommandOption.ValueSuffix (["-o"], TokenEncoding.SimpleString (Some Typeformat.FilePath)); description = "write output in file"; location = "options" };
    anchors = StringMap.empty
      |> StringMap.add "options" [];
    args = StringMap.empty
      |> StringMap.add "input" { Feature.Argument.encoding = Feature.Argument.ArgList ((TokenEncoding.SimpleString (Some Typeformat.FilePath)),0,1); description = "Input file/directory" };
    description = "Compute graphviz visualization";
    path = "dot ?options [<input>]";
  }

let dot : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "dot";
    version = "UNVERSIONED";
    description = "Compute graphviz visualization";
  };
  features = dotFeatures;
}
