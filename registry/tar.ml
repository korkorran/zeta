open Ocf

let tarFeatures = StringMap.empty
  |> Feature.addFeature "create_archive" {
    options = StringMap.empty
    |> StringMap.add "create" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.Once (Feature.CommandOption.SingleToken (TokenEncoding.FixedString ["-c"])); description = "Create archive"; location = "options" })
    |> StringMap.add "verbose" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.Once (Feature.CommandOption.SingleToken (TokenEncoding.FixedString ["-v"])); description = "Verbose output"; location = "options" });
    anchors = StringMap.empty
    |> StringMap.add "options" [ Feature.OptionAnchor.ExactLocation ];
    args = StringMap.empty
    |> StringMap.add "files" (Feature.Argument.Any { encoding = Feature.Argument.Between (1, 10, TokenEncoding.FormattedString (Some TokenEncoding.FilePath)); description = "Files to include in the archive" });
    description = "Create a tar archive";
    path = FeaturePath.Token.[Directive "tar"; Anchor "options"; ArgumentLocation "files"];
  }

let tar : Ocf.Command.command = {
  info = {
    commandName = "tar";
    version = "builtin";
    description = "Create or extract files from a tar archive";
  };
  features = tarFeatures;
}