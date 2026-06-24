module Token = struct 
  type t = Directive of string 
    | SingleOptionAnchor of string 
    | Anchor of string
    | SingleArgumentLocation of string
    | ArgumentLocation of string
  [@@deriving show]

  let token_to_string = function
    | Directive s -> s
    | SingleOptionAnchor s -> "?" ^ s
    | Anchor s -> "[?" ^ s ^ "]"
    | SingleArgumentLocation s -> "<" ^ s ^ ">"
    | ArgumentLocation s -> "[<" ^ s ^ ">]"

  let token_of_string s =
      if String.length s = 0 then
        failwith "Empty token string"
      else
        match s.[0] with
        | '?' -> SingleOptionAnchor (String.sub s 1 (String.length s - 1))
        | '[' ->
            if String.length s >= 3 && s.[1] = '?' && s.[String.length s - 1] = ']' then
              Anchor (String.sub s 2 (String.length s - 3))
            else if String.length s >= 4 && s.[1] = '<' && s.[String.length s - 2] = '>' && s.[String.length s - 1] = ']' then
              ArgumentLocation (String.sub s 2 (String.length s - 4))
            else
              failwith ("Invalid token: " ^ s)
        | '<' ->
            if String.length s >= 3 && s.[String.length s - 1] = '>' then
              SingleArgumentLocation (String.sub s 1 (String.length s - 2))
            else
              failwith ("Invalid argument token: " ^ s)
        | _ -> Directive s

  let compare t1 t2 =
    match (t1, t2) with
    | (Directive s1, Directive s2) -> String.compare s1 s2
    | (Anchor s1, Anchor s2) -> String.compare s1 s2
    | (ArgumentLocation s1, ArgumentLocation s2) -> String.compare s1 s2
    | (SingleArgumentLocation s1, SingleArgumentLocation s2) -> String.compare s1 s2
    | (SingleOptionAnchor s1, SingleOptionAnchor s2) -> String.compare s1 s2
    | (Directive _, _) -> -1
    | (_, Directive _) -> 1
    | (Anchor _, _) -> -1
    | (_, Anchor _) -> 1
    | (SingleOptionAnchor _, _) -> -1
    | (_, SingleOptionAnchor _) -> 1
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