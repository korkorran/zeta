open Opencommand

let lsFeatures = StringMap.empty
  |> Feature.addFeature "list_directory_contents" {
    options = StringMap.empty
      |> StringMap.add "all" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-a"]; description = "Show all files"; location = "options" }
      |> StringMap.add "long" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-l"]; description = "Long format"; location = "options" }
      |> StringMap.add "verbose" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" };
    optionsLocations = StringMap.empty
      |> StringMap.add "options" { Feature.from = 1; Feature.to_ = 10 };
    args = StringMap.empty
      |> StringMap.add "directories" { Feature.Argument.encoding = Feature.Argument.ArgList ((TokenEncoding.SimpleString (Some Typeformat.FilePath)),0,10); description = "Directories to list" };
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
