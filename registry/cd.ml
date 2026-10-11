open Ocf

let cdFeatures = StringMap.empty
  |> Feature.addFeature "change_directory" {
    options = StringMap.empty;
    anchors = StringMap.empty;
    args = StringMap.empty
      |> StringMap.add "directory" (Feature.Argument.Any { encoding = Feature.Argument.Single (TokenEncoding.SimpleString (Some TokenEncoding.FilePath)); description = "Directory to change to" });
    description = "Change the current directory";
    path = FeaturePath.Token.[Directive "cd"; ArgumentLocation "directory"];
  }

let cd : Ocf.Command.command = {
  info = {
    commandName = "cd";
    version = "builtin";
    description = "Change the current directory";
  };
  features = cdFeatures;
}
