open Ocf

let registry : Ocf.Command.command StringMap.t = StringMap.empty
  |> StringMap.add "git" Git.git
  |> StringMap.add "mv" Mv.mv
  |> StringMap.add "cd" Cd.cd
  |> StringMap.add "pwd" Pwd.pwd
  |> StringMap.add "ls" Ls.ls
  |> StringMap.add "dot" Dot.dot
  |> StringMap.add "tar" Tar.tar
  |> StringMap.add "gcc" Gcc.gcc