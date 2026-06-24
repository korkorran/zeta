open Opencommand

let mvFeatures = StringMap.empty
  |> Feature.addFeature "move_or_rename" {
    options = StringMap.empty
      |> StringMap.add "force" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-f"]; description = "Force move"; location = "options" }
      |> StringMap.add "verbose" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" }
      |> StringMap.add "interactive" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-i"]; description = "Prompt before move"; location = "options" };
    anchors = StringMap.empty
      |> StringMap.add "options" [];
    args = StringMap.empty
      |> StringMap.add "source" { Feature.Argument.encoding = Feature.Argument.ArgList ((TokenEncoding.SimpleString (Some Typeformat.SingleWord)),1,3); description = "Source file/directory" }
      |> StringMap.add "destination" { Feature.Argument.encoding = Feature.Argument.Single (TokenEncoding.SimpleString (Some Typeformat.SingleWord)); description = "Destination file/directory" };
    description = "Move or rename a file, a directory, or a symlink";
    path = "mv [?options] [<source>] <destination>";
  }

let mv : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "mv";
    version = "8.32";
    description = "Move or rename a file, a directory, or a symlink";
  };
  features = mvFeatures;
}