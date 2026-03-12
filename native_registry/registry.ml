open Opencommand.Utils
let registry : Opencommand.Command.command StringMap.t = StringMap.empty
  |> StringMap.add "git" Git.git
  |> StringMap.add "mv" Mv.mv