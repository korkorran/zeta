open Ocf

let lsFeatures = StringMap.empty
  |> Feature.addFeature "list_directory_contents" {
    options = StringMap.empty
      |> StringMap.add "all" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-a"]; description = "Show all files"; location = "options" })
      |> StringMap.add "long" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-l"]; description = "Long format"; location = "options" })
      |> StringMap.add "verbose" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" });
    anchors = StringMap.empty
      |> StringMap.add "options" [];
    args = StringMap.empty
      |> StringMap.add "directories" (Feature.Argument.Any { encoding = Feature.Argument.Between (0, 10, TokenEncoding.SimpleString (Some TokenEncoding.FilePath)); description = "Directories to list" });
    description = "List directory contents";
    path = FeaturePath.Token.[Directive "ls"; Anchor "options"; ArgumentLocation "directories"];
  }

let ls : Ocf.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "ls";
    version = "8.32";
    description = "List directory contents";
  };
  features = lsFeatures;
}
