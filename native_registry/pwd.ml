open Opencommand

let pwdFeatures = StringMap.empty
  |> Feature.addFeature "print_working_directory" {
    options = StringMap.empty
    |> StringMap.add "physicPath" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-P"]; description = "Follow the physical directory structure"; location = "options" }
    |> StringMap.add "logicalPath" { Feature.CommandOption.encoding = Feature.CommandOption.SingleString ["-L"]; description = "Follow the logical directory structure"; location = "options" };
    optionsLocations = StringMap.empty
    |> StringMap.add "options" { Feature.from = 1; Feature.to_ = 10 };
    args = StringMap.empty;
    description = "Print the name of the current directory";
    path = "pwd [?options]";
  }

let pwd : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "pwd";
    version = "builtin";
    description = "Print the name of the current directory";
  };
  features = pwdFeatures;
}
