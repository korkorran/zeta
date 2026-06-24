open Opencommand.Command
open Opencommand

let dotFeatures = emptyCommandFeatures
  |> addFeature "generate_visualization" {
    options = StringMap.empty
      |> StringMap.add "output" { CommandOption.encoding = CommandOption.ValueSuffix (["-o"], SimpleString (Some FilePath)); description = "write output in file"; location = "options" };
    optionsLocations = StringMap.empty
      |> StringMap.add "options" { from = 1; to_ = 10 };
    args = StringMap.empty
      |> StringMap.add "input" { Argument.encoding = Argument.ArgList ((SimpleString (Some FilePath)),0,1); description = "Input file/directory" };
    description = "Compute graphviz visualization";
    path = "dot [?options] [<input>]";
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
