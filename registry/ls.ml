open Ocf

let lsFeatures = StringMap.empty
  |> Feature.addFeature "list_directory_contents" {
    options = StringMap.empty
      |> StringMap.add "all" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleToken (TokenEncoding.FixedString ["-a"]); description = "Show all files"; location = "options" })
      |> StringMap.add "long" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleToken (TokenEncoding.FixedString ["-l"]); description = "Long format"; location = "options" })
      |> StringMap.add "verbose" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleToken (TokenEncoding.FixedString ["-v"]); description = "Verbose output"; location = "options" });
    anchors = StringMap.empty
      |> StringMap.add "options" [];
    args = StringMap.empty
      |> StringMap.add "directories" (Feature.Argument.Any { encoding = Feature.Argument.Between (0, 10, TokenEncoding.FormattedString (Some TokenEncoding.FilePath)); description = "Directories to list" });
    description = "List directory contents";
    path = FeaturePath.Token.[Directive "ls"; Anchor "options"; ArgumentLocation "directories"];
  }

let ls : Ocf.Command.command = {
  info = {
    commandName = "ls";
    version = "8.32";
    description = "List directory contents";
  };
  features = lsFeatures;
}
