open Ocf

let mvFeatures = StringMap.empty
  |> Feature.addFeature "move_or_rename" {
    options = StringMap.empty
      |> StringMap.add "force" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-f"]; description = "Force move"; location = "options" })
      |> StringMap.add "verbose" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" })
      |> StringMap.add "interactive" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-i"]; description = "Prompt before move"; location = "options" });
    anchors = StringMap.empty
      |> StringMap.add "options" [Feature.OptionAnchor.ExactLocation];
    args = StringMap.empty
      |> StringMap.add "source" (Feature.Argument.Any { encoding = Feature.Argument.Over (1, TokenEncoding.SimpleString (Some TokenEncoding.SingleWord)); description = "Source file/directory" })
      |> StringMap.add "destination" (Feature.Argument.Any { encoding = Feature.Argument.Single (TokenEncoding.SimpleString (Some TokenEncoding.SingleWord)); description = "Destination file/directory" });
    description = "Move or rename a file, a directory, or a symlink";
    path = FeaturePath.Token.[Directive "mv"; Anchor "options"; ArgumentLocation "source"; SingleArgumentLocation "destination"];
  }

let mv : Ocf.Command.command = {
  info = {
    commandName = "mv";
    version = "8.32";
    description = "Move or rename a file, a directory, or a symlink";
  };
  features = mvFeatures;
}