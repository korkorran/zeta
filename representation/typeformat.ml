open Ocf

type 'a typeformat = TUnit : unit typeformat
                | TInt : int typeformat
                | TFloat : float typeformat
                | TBool : bool typeformat
                | TString : TokenEncoding.stringFormat option -> string typeformat
                | TList : 'a typeformat -> 'a list typeformat
                | TOption : 'a typeformat -> 'a option typeformat
                | TPair : 'a typeformat * 'b typeformat -> ('a * 'b) typeformat
                | TEnum : string list -> string typeformat
(*                 | TRecord of (string * typeformat) list
                | TVariant of (string * typeformat option) list *)

type any = Any : 'a typeformat -> any

(* a configuration value, typed by its typeformat *)
type value = Value : 'a typeformat * 'a -> value

(* witness that two typeformats describe the same OCaml type *)
let rec equal_type : type a b. a typeformat -> b typeformat -> (a, b) Type.eq option = fun tf1 tf2 ->
  match tf1, tf2 with
  | TUnit, TUnit -> Some Type.Equal
  | TInt, TInt -> Some Type.Equal
  | TFloat, TFloat -> Some Type.Equal
  | TBool, TBool -> Some Type.Equal
  | TString _, TString _ -> Some Type.Equal
  | TString _, TEnum _ -> Some Type.Equal
  | TEnum _, TString _ -> Some Type.Equal
  | TEnum _, TEnum _ -> Some Type.Equal
  | TList tf1, TList tf2 -> (match equal_type tf1 tf2 with Some Type.Equal -> Some Type.Equal | None -> None)
  | TOption tf1, TOption tf2 -> (match equal_type tf1 tf2 with Some Type.Equal -> Some Type.Equal | None -> None)
  | TPair (a1, b1), TPair (a2, b2) -> (match equal_type a1 a2, equal_type b1 b2 with
    | Some Type.Equal, Some Type.Equal -> Some Type.Equal
    | _ -> None)
  | _, _ -> None

(* two values are equal when they have the same OCaml type and are structurally equal *)
let equal_value (Value (tf1, v1)) (Value (tf2, v2)) =
  match equal_type tf1 tf2 with
  | Some Type.Equal -> v1 = v2
  | None -> false

let rec pp_typeformat : type a. Format.formatter -> a typeformat -> unit = fun fmt tf ->
  match tf with
  | TUnit -> Format.fprintf fmt "TUnit"
  | TInt -> Format.fprintf fmt "TInt"
  | TFloat -> Format.fprintf fmt "TFloat"
  | TBool -> Format.fprintf fmt "TBool"
  | TString format_opt -> Format.fprintf fmt "TString (%a)" (Format.pp_print_option ~none:(fun fmt () -> Format.fprintf fmt "None") TokenEncoding.pp_stringFormat) format_opt
  | TList tf -> Format.fprintf fmt "TList (%a)" pp_typeformat tf
  | TOption tf -> Format.fprintf fmt "TOption (%a)" pp_typeformat tf
  | TPair (a, b) -> Format.fprintf fmt "TPair (%a, %a)" pp_typeformat a pp_typeformat b
  | TEnum options -> Format.fprintf fmt "TEnum [%s]" (String.concat "; " options)

let pp_any fmt (Any tf) = pp_typeformat fmt tf

type validationError = 
  | TypeMismatch
  | FormatError of any
[@@deriving show]

let validationError_to_string = function
  | TypeMismatch -> "Type mismatch"
  | FormatError (Any tf) -> 
    let describe : type a. a typeformat -> string = function
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
      | TPair _ -> "Value must be a pair"
      | TEnum options -> "Value must be one of: " ^ String.concat ", " options
    in
    describe tf

let rec validate : type a. a typeformat -> a -> (unit, validationError) Result.t = fun typeformat value ->
  match typeformat with
  | TUnit | TInt | TFloat | TBool -> Result.Ok ()
  | TString format_opt -> let open Tyre in (match format_opt with
    | Some TokenEncoding.QuotedString -> 
      let re = start *> (char '"') *> (rep (charset (Charset.not (Charset.char '"')))) <* (char '"') <* stop in
        if execp (compile re) value then Result.Ok () else Result.Error (FormatError (Any typeformat))
    | Some TokenEncoding.SingleWord -> let re = start *> (rep1 (charset (Charset.not (Charset.blank)))) <* stop in
        if execp (compile re) value then Result.Ok () else Result.Error (FormatError (Any typeformat))
    | Some TokenEncoding.FilePath -> let re = start *> (rep1 (charset (Charset.union [(Charset.alnum); (Charset.punct)]))) <* stop in
        if execp (compile re) value then Result.Ok () else Result.Error (FormatError (Any typeformat))
    | None -> Result.Ok ())
  | TList tf ->
    if List.for_all (fun v -> Result.is_ok (validate tf v)) value
      then Result.Ok () else Result.Error (FormatError (Any typeformat))
  | TOption tf -> (match value with Some v -> validate tf v | None -> Result.Ok ())
  | TPair (tfa, tfb) -> let (a, b) = value in
    if Result.is_ok (validate tfa a) && Result.is_ok (validate tfb b)
      then Result.Ok () else Result.Error (FormatError (Any typeformat))
  | TEnum options -> if List.mem value options then Result.Ok () else Result.Error (FormatError (Any typeformat))

(* validates a configuration value against the expected typeformat *)
let validate_value : type a. a typeformat -> value -> (unit, validationError) Result.t = fun typeformat (Value (tf, v)) ->
  match equal_type typeformat tf with
  | Some Type.Equal -> validate typeformat v
  | None -> Result.Error TypeMismatch

let rec pp : type a. a typeformat -> Format.formatter -> a -> unit = fun typeformat fmt value ->
  match typeformat with
  | TUnit -> Format.fprintf fmt "unit"
  | TInt -> Format.fprintf fmt "%d" value
  | TFloat -> Format.fprintf fmt "%f" value
  | TBool -> Format.fprintf fmt "%b" value
  | TString _ -> Format.fprintf fmt "%s" value
  | TEnum _ -> Format.fprintf fmt "%s" value
  | TList tf ->
      List.iteri (fun i v ->
        if i > 0 then Format.fprintf fmt " ";
        pp tf fmt v) value
  | TOption tf ->
      (match value with
       | Some v -> pp tf fmt v
       | None -> Format.fprintf fmt "None")
  | TPair (tfa, tfb) -> let (a, b) = value in
      Format.fprintf fmt "(%a, %a)" (pp tfa) a (pp tfb) b

let pp_value fmt (Value (tf, v)) = pp tf fmt v

let typeformat_of_tokenEncoding : type a. a TokenEncoding.tokenEncoding -> a typeformat = function
  | TokenEncoding.FixedString _ -> TUnit
  | TokenEncoding.FormattedString fmt -> TString fmt
  | TokenEncoding.Integer -> TInt
  | TokenEncoding.Enum options -> TEnum options

(* reads the value of a token written with the given encoding, None if the token does not match it *)
let value_of_token : type a. a TokenEncoding.tokenEncoding -> string -> a option = fun encoding token ->
  match encoding with
  | TokenEncoding.FixedString strings -> if List.mem token strings then Some () else None
  | TokenEncoding.FormattedString _ -> Some token
  | TokenEncoding.Integer -> int_of_string_opt token
  | TokenEncoding.Enum options -> if List.mem token options then Some token else None

(* writes a value as a token with the given encoding *)
let token_of_value : type a. a TokenEncoding.tokenEncoding -> a -> string = fun encoding value ->
  match encoding with
  | TokenEncoding.FixedString strings -> List.hd strings
  | TokenEncoding.FormattedString _ -> value
  | TokenEncoding.Integer -> string_of_int value
  | TokenEncoding.Enum _ -> value
