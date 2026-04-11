open Opencommand.Command
open Opencommand.Utils

let pwdPaths = emptyCommandPaths
  |> addPath "pwd [?options]" {
    options = StringMap.empty
    |> StringMap.add "physicPath" { CommandOption.encoding = CommandOption.SingleString ["-P"]; description = "Follow the physical directory structure"; location = "options" }
    |> StringMap.add "logicalPath" { CommandOption.encoding = CommandOption.SingleString ["-L"]; description = "Follow the logical directory structure"; location = "options" };
    optionsLocations = StringMap.empty
    |> StringMap.add "options" { from = 1; to_ = 10 };
    args = StringMap.empty;
    description = "Print the name of the current directory"
  }

let pwd : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "pwd";
    version = "builtin";
    description = "Print the name of the current directory";
  };
  paths = pwdPaths;
}