open Opencommand

type typeformat = TUnit
                | TInt
                | TFloat
                | TBool
                | TString of TokenEncoding.stringFormat option
                | TList of typeformat
                | TOption of typeformat
                | TTuple of typeformat list
                | TEnum of string list
(*                 | TRecord of (string * typeformat) list
                | TVariant of (string * typeformat option) list *)
[@@deriving yojson, show]

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

type validationError = 
  | TypeMismatch
  | FormatError of typeformat
[@@deriving show]

let validationError_to_string = function
  | TypeMismatch -> "Type mismatch"
  | FormatError tf -> match tf with
    | TUnit -> "Value must be 'unit'"
    | TInt -> "Value must be an integer"
    | TFloat -> "Value must be a float"
    | TBool -> "Value must be 'true' or 'false'"
    | TString (Some TokenEncoding.QuotedString) -> "Value must be a quoted string"
    | TString (Some TokenEncoding.SingleWord) -> "Value must be a single word without spaces"
    | TString (Some TokenEncoding.FilePath) -> "Value must be a file path without spaces"
    | TString None -> "Value must be a string"
    | TList _ -> "Value must be a list"
    | TOption _ -> "Value must be an option (None or Some value)"
    | TTuple _ -> "Value must be a tuple"
    | TEnum options -> "Value must be one of: " ^ String.concat ", " options

    
let rec validate typeformat value =
  match typeformat, value with
  | TUnit, VUnit -> Result.Ok ()
  | TInt, VInt _ -> Result.Ok ()
  | TFloat, VFloat _ -> Result.Ok ()
  | TBool, VBool _ -> Result.Ok ()
  | TString format_opt, VString s -> let open Tyre in (match format_opt with
    | Some TokenEncoding.QuotedString -> 
      let re = start *> (char '"') *> (rep (charset (Charset.not (Charset.char '"')))) <* (char '"') <* stop in
        if execp (compile re) s then Result.Ok () else Result.Error (FormatError (TString (Some TokenEncoding.QuotedString)))
    | Some TokenEncoding.SingleWord -> let re = start *> (rep1 (charset (Charset.not (Charset.blank)))) <* stop in
        if execp (compile re) s then Result.Ok () else Result.Error (FormatError (TString (Some TokenEncoding.SingleWord)))
    | Some TokenEncoding.FilePath -> let re = start *> (rep1 (charset (Charset.union [(Charset.alnum); (Charset.punct)]))) <* stop in
        if execp (compile re) s then Result.Ok () else Result.Error (FormatError (TString (Some TokenEncoding.FilePath)))
    | None -> Result.Ok ())
  | TList tf, VList vs -> let error_message = ref None in
    if List.for_all (fun v -> match validate tf v with Result.Ok () -> true| Result.Error m -> error_message := Some m; false) vs
      then Result.Ok () else Result.Error (FormatError (TList tf))
  | TOption tf, VOption v -> (match v with Some v' -> validate tf v' | None -> Result.Ok ())
  | TTuple tfs, VTuple vs -> if List.length tfs = List.length vs then
    let error_message = ref None in
    let validate_pair tf v = match validate tf v with Result.Ok () -> true | Result.Error m -> error_message := Some m; false in
    ( if List.for_all2 validate_pair tfs vs then Result.Ok () else Result.Error (FormatError (TTuple tfs)) )
    else Result.Error (FormatError (TTuple tfs))
  | TEnum options, VEnum v -> if List.exists ((=) v) options then Result.Ok () else Result.Error (FormatError (TEnum options))
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
  | _, _ -> Result.Error TypeMismatch

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

let typeformat_of_tokenEncoding : type a. a TokenEncoding.tokenEncoding -> typeformat = function
  | TokenEncoding.SimpleString fmt -> TString fmt
  | TokenEncoding.Integer -> TInt
  | TokenEncoding.Enum options -> TEnum options
