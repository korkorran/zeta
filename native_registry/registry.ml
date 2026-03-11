open Opencommand.Utils
let registry = StringMap.empty
  |> StringMap.add "git" Git.git
  |> StringMap.add "mv" Mv.mv