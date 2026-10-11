

type info = {
  commandName: string;
  version: string;
  description: string;
}

type command = {
  info: info;
  features: Feature.featureMap;
}
(* type command = head EncodedPathMap.t

let parseEncodedCommand (c:rawCommand) : command =
  StringMap.fold (fun path head acc ->
    let encodedPath = EncodedPath.from_string path in
    EncodedPathMap.add encodedPath head acc
  ) c EncodedPathMap.empty *)


