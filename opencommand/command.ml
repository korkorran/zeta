

module CommandOption = struct

  type encoding =
  | SingleString : string list -> encoding
  | ValueSuffix : string list * ('a TokenEncoding.tokenEncoding) -> encoding

  type t = {
    encoding: encoding;
    description: string;
    location: string;
  }
end


module Argument = struct
  type encoding = 
    | Single : 'a TokenEncoding.tokenEncoding -> encoding
    | ArgList : ('a TokenEncoding.tokenEncoding*int*int) -> encoding  (* min, max *)

  type t = {
    encoding: encoding;
    description: string;
  }
end

type optionLocation = {
  from : int;
  to_ : int;
}

type head = {
  options: CommandOption.t StringMap.t;
  optionsLocations: optionLocation StringMap.t;
  args: Argument.t StringMap.t;
  description: string;
  path: string;
}

type commandFeatures = head StringMap.t

type info = {
  commandName: string;
  version: string;
  description: string;
}

type command = {
  opencommand_version: string;
  info: info;
  features: commandFeatures;
}
(* type command = head EncodedPathMap.t

let parseEncodedCommand (c:rawCommand) : command =
  StringMap.fold (fun path head acc ->
    let encodedPath = EncodedPath.from_string path in
    EncodedPathMap.add encodedPath head acc
  ) c EncodedPathMap.empty *)

let emptyCommandFeatures : commandFeatures = StringMap.empty

let addFeature path (head:head) cmdFeatures =
  StringMap.add path head cmdFeatures
