open Opencommand.Command
open Opencommand.Utils

let cdPaths = emptyCommandPaths
  |> addPath "cd <directory>" {
    options = StringMap.empty;
    optionsLocations = StringMap.empty;
    args = StringMap.empty
      |> StringMap.add "directory" { Argument.encoding = Argument.Single (TString (Some FilePath)); description = "Directory to change to" };
    description = "Change the current directory"
  }

let cd : Opencommand.Command.command = {
  opencommand_version = "0.0.1";
  info = {
    commandName = "cd";
    version = "builtin";
    description = "Change the current directory";
  };
  paths = cdPaths;
}