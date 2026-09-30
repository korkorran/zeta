open Opencommand

let cdFeatures = StringMap.empty
  |> Feature.addFeature "change_directory" {
    options = StringMap.empty;
    anchors = StringMap.empty;
    args = StringMap.empty
      |> StringMap.add "directory" { Feature.Argument.encoding = Feature.Argument.Single (TokenEncoding.SimpleString (Some Typeformat.FilePath)); description = "Directory to change to" };
    description = "Change the current directory";
    path = FeaturePath.Token.[Directive "cd"; SingleArgumentLocation "directory"];
  }

let cd : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "cd";
    version = "builtin";
    description = "Change the current directory";
  };
  features = cdFeatures;
}
