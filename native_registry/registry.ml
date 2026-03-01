module StringMap = Map.Make(String)

let registry = StringMap.empty
  |> StringMap.add "git" Git.git
  |> StringMap.add "mv" Mv.mv