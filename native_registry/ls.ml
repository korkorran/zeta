open Opencommand.Command
open Opencommand

let lsFeatures = emptyCommandFeatures
  |> addFeature "list_directory_contents" {
    options = StringMap.empty
      |> StringMap.add "all" { CommandOption.encoding = CommandOption.SingleString ["-a"]; description = "Show all files"; location = "options" }
      |> StringMap.add "long" { CommandOption.encoding = CommandOption.SingleString ["-l"]; description = "Long format"; location = "options" }
      |> StringMap.add "verbose" { CommandOption.encoding = CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" };
    optionsLocations = StringMap.empty
      |> StringMap.add "options" { from = 1; to_ = 10 };
    args = StringMap.empty
      |> StringMap.add "directories" { Argument.encoding = Argument.ArgList ((SimpleString (Some FilePath)),0,10); description = "Directories to list" };
    description = "List directory contents";
    path = "ls [?options] [<directories>]";
  }

let ls : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "ls";
    version = "8.32";
    description = "List directory contents";
  };
  features = lsFeatures;
}
