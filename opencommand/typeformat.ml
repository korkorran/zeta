type stringFormat = | QuotedString | SingleWord
[@@deriving yojson]

type typeformat = TUnit
                | TInt
                | TFloat
                | TBool
                | TString of stringFormat option
                | TList of typeformat
                | TOption of typeformat
                | TTuple of typeformat list
                | TEnum of string list
(*                 | TRecord of (string * typeformat) list
                | TVariant of (string * typeformat option) list *)
[@@deriving yojson]

type value = VUnit
           | VInt of int
           | VFloat of float
           | VBool of bool
           | VString of string
           | VList of value list
           | VOption of value option
           | VTuple of value list
           | VEnum of string
(*            | VRecord of (string * value) list
           | VVariant of string * value option *)
[@@deriving yojson]

    
let rec validate typeformat value =
  match typeformat, value with
  | TUnit, VUnit -> Result.Ok ()
  | TInt, VInt _ -> Result.Ok ()
  | TFloat, VFloat _ -> Result.Ok ()
  | TBool, VBool _ -> Result.Ok ()
  | TString format_opt, VString s -> let open Tyre in (match format_opt with
    | Some QuotedString -> 
      let re = start *> (char '"') *> (rep (charset (Charset.not (Charset.char '"')))) <* (char '"') <* stop in
        if execp (compile re) s then Result.Ok () else Result.Error "Value must be a quoted string"
    | Some SingleWord -> let re = start *> (rep1 (charset (Charset.not (Charset.blank)))) <* stop in
        if execp (compile re) s then Result.Ok () else Result.Error "Value must be a single word without spaces"
    | None -> Result.Ok ())
  | TList tf, VList vs -> let error_message = ref "" in
    if List.for_all (fun v -> match validate tf v with Result.Ok () -> true| Result.Error m -> error_message := m; false) vs
      then Result.Ok () else Result.Error ("List element type mismatch: " ^ !error_message)
  | TOption tf, VOption v -> (match v with Some v' -> validate tf v' | None -> Result.Ok ())
  | TTuple tfs, VTuple vs -> if List.length tfs = List.length vs then
    let error_message = ref "" in
    let validate_pair tf v = match validate tf v with Result.Ok () -> true | Result.Error m -> error_message := m; false in
    ( if List.for_all2 validate_pair tfs vs then Result.Ok () else Result.Error ("element type mismatch: " ^ !error_message ) )
    else Result.Error "Tuple length mismatch"
  | TEnum options, VEnum v -> if List.exists ((=) v) options then Result.Ok () else Result.Error ("Value must be one of: " ^ String.concat ", " options)
(*   | TRecord fields, VRecord values ->
      let field_map = List.fold_left (fun acc (name, tf) -> (name, tf) :: acc) [] fields in
      List.for_all (fun (name, v) ->
        match List.assoc_opt name field_map with
        | Some tf -> validate tf v
        | None -> false) values
  | TVariant variants, VVariant (name, v_opt) ->
      (match List.assoc_opt name variants with
       | Some tf_opt ->
           (match tf_opt, v_opt with
            | Some tf, Some v -> validate tf v
            | None, None -> true
            | _, _ -> false)
       | None -> false) *)
  | _, _ -> Result.Error "Type mismatch"

  let rec pp_value fmt = function
    | VUnit -> Format.fprintf fmt "unit"
    | VInt i -> Format.fprintf fmt "%d" i
    | VFloat f -> Format.fprintf fmt "%f" f
    | VBool b -> Format.fprintf fmt "%b" b
    | VString s -> Format.fprintf fmt "%s" s
    | VList vs ->
        List.iteri (fun i v ->
          if i > 0 then Format.fprintf fmt " ";
          pp_value fmt v) vs
    | VOption v_opt ->
        (match v_opt with
         | Some v -> pp_value fmt v
         | None -> Format.fprintf fmt "None")
    | VTuple vs ->
        Format.fprintf fmt "(";
        List.iteri (fun i v ->
          if i > 0 then Format.fprintf fmt ", ";
          pp_value fmt v) vs;
        Format.fprintf fmt ")"
    | VEnum s -> Format.fprintf fmt "%s" s


let value_of_token typeformat token =
  match typeformat, token with
  | TUnit, "unit" -> Some VUnit
  | TInt, s -> (try Some (VInt (int_of_string s)) with _ -> None)
  | TFloat, s -> (try Some (VFloat (float_of_string s)) with _ -> None)
  | TBool, "true" -> Some (VBool true)
  | TBool, "false" -> Some (VBool false)
  | TString _, s -> Some (VString s)
  | TEnum options, s when List.exists ((=) s) options -> Some (VEnum s)
  | _, _ -> None