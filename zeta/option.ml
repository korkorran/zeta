open Ocf
open Repr

let getOptionsFromAnchor (feature:Feature.feature) (availableAnchor:string) : (string * Feature.CommandOption.any) list =
  StringMap.bindings feature.options
  |> List.filter (fun (_, Feature.CommandOption.Any opt) -> opt.location = availableAnchor)

(* reads one occurrence of an option at the head of the entry: its value, the consumed tokens and the rest of the entry *)
let readOccurrence : type a. a Feature.CommandOption.written -> string list -> (a * string list * string list) option = fun written restEntry ->
  match written, restEntry with
  | Feature.CommandOption.SingleToken encoding, he::te -> (
    match Typeformat.value_of_token encoding he with
    | Some value -> Some (value, [he], te)
    | None -> None)
  | Feature.CommandOption.ValueNextToken (keyEncoding, encoding), h1::h2::te -> (
    match Typeformat.value_of_token keyEncoding h1, Typeformat.value_of_token encoding h2 with
    | Some (), Some value -> Some (value, [h1; h2], te)
    | _ -> None)
  | _, _ -> None

(* appends [value] to the values of a repeatable option already in the configuration *)
let appendValue : type a. a Typeformat.typeformat -> Typeformat.value option -> a -> Typeformat.value = fun typeformat previous value ->
  let listTypeformat = Typeformat.TList typeformat in
  match previous with
  | None -> Typeformat.Value (listTypeformat, [value])
  | Some (Typeformat.Value (tf, values)) -> (
    match Typeformat.equal_type listTypeformat tf with
    | Some Type.Equal -> Typeformat.Value (listTypeformat, values @ [value])
    | None -> failwith "This should not happen, a repeatable option should always be associated with a list of its encoding in the configuration")

let evalOption 
  (id, Feature.CommandOption.Any option: string* Feature.CommandOption.any) 
  (restEntry: string list) 
  (currentConfig: Configuration.configuration) 
  : (Configuration.configuration * string list * string list) option =
  let withOption value = {Configuration.options=StringMap.add id value currentConfig.options; args=currentConfig.args} in
  match option.encoding with
  | Feature.CommandOption.Once written -> (
    let typeformat = Typeformat.typeformat_of_tokenEncoding (Feature.CommandOption.valueEncoding written) in
    match readOccurrence written restEntry with
    (* the last occurrence wins *)
    | Some (value, tokens, rest) -> Some (withOption (Typeformat.Value (Typeformat.TOption typeformat, Some value)), tokens, rest)
    | None -> None)
  | Feature.CommandOption.Repeatable written -> (
    let typeformat = Typeformat.typeformat_of_tokenEncoding (Feature.CommandOption.valueEncoding written) in
    match readOccurrence written restEntry with
    | Some (value, tokens, rest) -> Some (withOption (appendValue typeformat (StringMap.find_opt id currentConfig.options) value), tokens, rest)
    | None -> None)
