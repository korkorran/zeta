open Opencommand

let tarFeatures = StringMap.empty
  |> Feature.addFeature "create_archive" {
    options = StringMap.empty
    |> StringMap.add "create" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-c"]; description = "Create archive"; location = "options" }
    |> StringMap.add "verbose" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" };
    anchors = StringMap.empty
    |> StringMap.add "options" [ Feature.OptionAnchor.ExactLocation ];
    args = StringMap.empty
    |> StringMap.add "files" { Feature.Argument.encoding = Feature.Argument.ArgList ((TokenEncoding.SimpleString (Some Typeformat.FilePath)),1,10); description = "Files to include in the archive" };
    description = "Create a tar archive";
    path = FeaturePath.Token.[Directive "tar"; Anchor "options"; ArgumentLocation "files"];
  }

let tar : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "tar";
    version = "builtin";
    description = "Create or extract files from a tar archive";
  };
  features = tarFeatures;
}