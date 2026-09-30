module Token = struct
  type t = Directive of string
    | Anchor of string
    | SingleArgumentLocation of string
    | ArgumentLocation of string
  [@@deriving show]

  let token_to_string = function
    | Directive s -> s
    | Anchor s -> "?" ^ s
    | SingleArgumentLocation s -> "<" ^ s ^ ">"
    | ArgumentLocation s -> "[<" ^ s ^ ">]"

  (* returns the content of s between prefix and suffix, if s is wrapped by them *)
  let unwrap prefix suffix s =
    let lp = String.length prefix and ls = String.length suffix and l = String.length s in
    if l > lp + ls && String.starts_with ~prefix s && String.ends_with ~suffix s then
      Some (String.sub s lp (l - lp - ls))
    else
      None

  let token_of_string s =
    if s = "" then failwith "Empty token string";
    match unwrap "?" "" s, unwrap "[<" ">]" s, unwrap "<" ">" s with
    | Some a, _, _ -> Anchor a
    | _, Some l, _ -> ArgumentLocation l
    | _, _, Some l -> SingleArgumentLocation l
    | _ when s.[0] = '?' || s.[0] = '[' || s.[0] = '<' -> failwith ("Invalid token: " ^ s)
    | _ -> Directive s

  let compare t1 t2 =
    let rank = function
      | Directive _ -> 0
      | Anchor _ -> 1
      | ArgumentLocation _ -> 2
      | SingleArgumentLocation _ -> 3
    in
    match (t1, t2) with
    | (Directive s1, Directive s2)
    | (Anchor s1, Anchor s2)
    | (ArgumentLocation s1, ArgumentLocation s2)
    | (SingleArgumentLocation s1, SingleArgumentLocation s2) -> String.compare s1 s2
    | _ -> Int.compare (rank t1) (rank t2)
end

module EncodedPath = struct
  type t = Token.t list

  let from_string str =
    String.split_on_char ' ' str |> List.map String.trim |> List.map Token.token_of_string

  let compare = List.compare Token.compare
end
