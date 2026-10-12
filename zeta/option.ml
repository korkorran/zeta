open Ocf
open Repr

let getOptionsFromAnchor (feature:Feature.feature) (availableAnchor:string) : (string * Feature.CommandOption.any) list =
  StringMap.bindings feature.options
  |> List.filter (fun (_, Feature.CommandOption.Any opt) -> opt.location = availableAnchor)

let evalOption 
  (id, Feature.CommandOption.Any option: string* Feature.CommandOption.any) 
  (restEntry: string list) 
  (currentConfig: Configuration.configuration) 
  : (Configuration.configuration * string list * string list) option =
  match option.encoding with
  | Feature.CommandOption.SingleString encodings -> (
    match restEntry with
    | [] -> None
    | he::te ->
        if List.exists (fun enc -> enc = he) encodings then
          let updatedOptions = StringMap.add id (Typeformat.Value (Typeformat.TOption Typeformat.TUnit, Some ())) currentConfig.options in
          Some ({Configuration.options=updatedOptions;args=currentConfig.args}, [he], te)
        else
          None )
  | Feature.CommandOption.ValueSuffix (prefixes, encoding) -> (
    match restEntry with
    | h1::h2::te -> 
      if List.exists (fun prefix -> prefix = h1) prefixes then
        let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
        match Typeformat.value_of_token typeformat h2 with
        | None -> None
        | Some value ->
        let updatedOptions = StringMap.add id (Typeformat.Value (Typeformat.TOption typeformat, Some value)) currentConfig.options in
        Some ({Configuration.options=updatedOptions;args=currentConfig.args}, [h1; h2], te)
      else
        None
    | _ -> None)