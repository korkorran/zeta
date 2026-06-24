open Opencommand.Command
open Opencommand
open Opencommand.TokenEncoding

let mvFeatures = emptyCommandFeatures
  |> addFeature "move_or_rename" {
    options = StringMap.empty
      |> StringMap.add "force" { CommandOption.encoding = CommandOption.SingleString ["-f"]; description = "Force move"; location = "options" }
      |> StringMap.add "verbose" { CommandOption.encoding = CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" }
      |> StringMap.add "interactive" { CommandOption.encoding = CommandOption.SingleString ["-i"]; description = "Prompt before move"; location = "options" };
    optionsLocations = StringMap.empty
      |> StringMap.add "options" { from = 1; to_ = 10 };
    args = StringMap.empty
      |> StringMap.add "source" { Argument.encoding = Argument.ArgList ((SimpleString (Some SingleWord)),1,3); description = "Source file/directory" }
      |> StringMap.add "destination" { Argument.encoding = Argument.Single (SimpleString (Some SingleWord)); description = "Destination file/directory" };
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