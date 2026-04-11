open Opencommand.Utils
let registry : Opencommand.Command.command StringMap.t = StringMap.empty
  |> StringMap.add "git" Git.git
  |> StringMap.add "mv" Mv.mv
  |> StringMap.add "cd" Cd.cd
  |> StringMap.add "pwd" Pwd.pwd
  |> StringMap.add "ls" Ls.ls