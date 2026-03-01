open Mapping.Command

let mv = emptyCommand
  |> addPath "mv [?options] <source> <destination>" {
    options = StringMap.empty
      |> StringMap.add "force" { CommandOption.encoding = CommandOption.SingleString ["-f"]; description = "Force move"; location = "options" }
      |> StringMap.add "verbose" { CommandOption.encoding = CommandOption.SingleString ["-v"]; description = "Verbose output"; location = "options" }
      |> StringMap.add "interactive" { CommandOption.encoding = CommandOption.SingleString ["-i"]; description = "Prompt before move"; location = "options" };
    optionsLocations = StringMap.empty
      |> StringMap.add "options" { from = 1; to_ = 10 };
    args = StringMap.empty
      |> StringMap.add "source" { Argument.encoding = Argument.SingleString; description = "Source file/directory" }
      |> StringMap.add "destination" { Argument.encoding = Argument.SingleString; description = "Destination file/directory" };
    description = "Move or rename a file, a directory, or a symlink"
  }