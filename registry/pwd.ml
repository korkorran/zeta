open Ocf

let pwdFeatures = StringMap.empty
  |> Feature.addFeature "print_working_directory" {
    options = StringMap.empty
    |> StringMap.add "physicPath" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-P"]; description = "Follow the physical directory structure"; location = "options" })
    |> StringMap.add "logicalPath" (Feature.CommandOption.Any { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-L"]; description = "Follow the logical directory structure"; location = "options" });
    anchors = StringMap.empty
    |> StringMap.add "options" [];
    args = StringMap.empty;
    description = "Print the name of the current directory";
    path = FeaturePath.Token.[Directive "pwd"; Anchor "options"];
  }

let pwd : Ocf.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "pwd";
    version = "builtin";
    description = "Print the name of the current directory";
  };
  features = pwdFeatures;
}
