module Token = struct 
  type t = Directive of string 
    | SingleOptionLocation of string 
    | OptionLocation of string
    | SingleArgumentLocation of string
    | ArgumentLocation of string
  [@@deriving show]

  let token_to_string = function
    | Directive s -> s
    | SingleOptionLocation s -> "?" ^ s
    | OptionLocation s -> "[?" ^ s ^ "]"
    | SingleArgumentLocation s -> "<" ^ s ^ ">"
    | ArgumentLocation s -> "[<" ^ s ^ ">]"

  let token_of_string s =
      if String.length s = 0 then
        failwith "Empty token string"
      else
        match s.[0] with
        | '?' -> OptionLocation (String.sub s 1 (String.length s - 1))
        | '[' ->
            if String.length s >= 3 && s.[1] = '?' && s.[String.length s - 1] = ']' then
              OptionLocation (String.sub s 2 (String.length s - 3))
            else if String.length s >= 4 && s.[1] = '<' && s.[String.length s - 2] = '>' && s.[String.length s - 1] = ']' then
              ArgumentLocation (String.sub s 2 (String.length s - 4))
            else
              failwith ("Invalid token: " ^ s)
        | '<' ->
            if String.length s >= 3 && s.[String.length s - 1] = '>' then
              ArgumentLocation (String.sub s 1 (String.length s - 2))
            else
              failwith ("Invalid argument token: " ^ s)
        | _ -> Directive s

  let compare t1 t2 =
    match (t1, t2) with
    | (Directive s1, Directive s2) -> String.compare s1 s2
    | (OptionLocation s1, OptionLocation s2) -> String.compare s1 s2
    | (ArgumentLocation s1, ArgumentLocation s2) -> String.compare s1 s2
    | (SingleArgumentLocation s1, SingleArgumentLocation s2) -> String.compare s1 s2
    | (SingleOptionLocation s1, SingleOptionLocation s2) -> String.compare s1 s2
    | (Directive _, _) -> -1
    | (_, Directive _) -> 1
    | (OptionLocation _, _) -> -1
    | (_, OptionLocation _) -> 1
    | (SingleOptionLocation _, _) -> -1
    | (_, SingleOptionLocation _) -> 1
    | (ArgumentLocation _, _) -> -1
    | (_, ArgumentLocation _) -> 1
end

module EncodedPath = struct
  type t = Token.t list

  let from_string str =
    let token_strings = String.split_on_char ' ' str |> List.map String.trim in
    List.map Token.token_of_string token_strings

  let compare t1 t2 =
    let rec aux l1 l2 =
      match (l1, l2) with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (h1::t1, h2::t2) ->
          let cmp = Token.compare h1 h2 in
          if cmp <> 0 then cmp else aux t1 t2
    in
    aux t1 t2
end

module EncodedPathMap = Map.Make(EncodedPath)
module StringMap = Map.Make(String)

module CommandOption = struct

  type encoding =
  | SingleString of string list
  | ValueSuffix of string list * Typeformat.typeformat

  type t = {
    encoding: encoding;
    description: string;
    location: string;
  }
end


module Argument = struct
  type encoding = SingleString | MultipleStrings of (int*int)  (* min, max *)
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
}

type rawCommand = head StringMap.t
type command = head EncodedPathMap.t

let parseEncodedCommand (c:rawCommand) : command =
  StringMap.fold (fun path head acc ->
    let encodedPath = EncodedPath.from_string path in
    EncodedPathMap.add encodedPath head acc
  ) c EncodedPathMap.empty

let emptyCommand : rawCommand = StringMap.empty

let addPath path (head:head) cmd =
  StringMap.add path head cmd

let getOptionsTemplate (head: head) : Typeformat.typeformat StringMap.t =
  head.options 
  |> StringMap.map (fun (opt) -> (
    match opt.CommandOption.encoding with
    | CommandOption.SingleString _ -> Typeformat.TUnit
    | CommandOption.ValueSuffix (_, tf) -> tf
    ))