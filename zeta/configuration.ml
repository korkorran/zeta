open Ocf


type configurationTemplate = {
  optionsTemplate: Typeformat.any StringMap.t;
  argsTemplate: Typeformat.any StringMap.t;
}

let getConfigurationTemplate (head: Feature.feature) : configurationTemplate =
  let typeformat_of_option_encoding : type a. a Feature.CommandOption.encoding -> a Typeformat.typeformat = function
    | Feature.CommandOption.SingleString _ -> Typeformat.TOption Typeformat.TUnit
    | Feature.CommandOption.ValueSuffix (_, enc) -> Typeformat.typeformat_of_tokenEncoding enc
  in
  let optionsTemplate = head.options 
  |> StringMap.map (fun (Feature.CommandOption.Any opt) -> Typeformat.Any (typeformat_of_option_encoding opt.encoding)) 
  in
  let typeformat_of_encoding : type a. a Feature.Argument.encoding -> a Typeformat.typeformat = function
    | Feature.Argument.Single enc -> Typeformat.typeformat_of_tokenEncoding enc
    | Feature.Argument.Optional enc -> Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding enc)
    | Feature.Argument.Over (_, enc) -> Typeformat.TList (Typeformat.typeformat_of_tokenEncoding enc)
    | Feature.Argument.Between (_, _, enc) -> Typeformat.TList (Typeformat.typeformat_of_tokenEncoding enc)
  in
  let argsTemplate = head.args
  |> StringMap.map (fun (Feature.Argument.Any arg) -> Typeformat.Any (typeformat_of_encoding arg.encoding)) in
  { optionsTemplate; argsTemplate }

let getEmptyConfiguration (template: configurationTemplate) : Entry.configuration =
  let options = StringMap.empty in
  let rec defaultValue : type a. a Typeformat.typeformat -> a = function
      | Typeformat.TUnit -> ()
      | TString _ -> ""
      | TInt -> 0
      | TFloat -> 0.0
      | TEnum options -> List.hd options
      | TOption _ -> None
      | TBool -> false
      | TPair (a, b) -> (defaultValue a, defaultValue b)
      | TList _ -> []
  in
  let args = template.argsTemplate |> StringMap.map (fun (Typeformat.Any tf) -> Typeformat.Value (tf, defaultValue tf)) in
  { Entry.options; args }

type optionOrArgument = Opt | Arg
[@@deriving show]
type itemError = Format of Typeformat.validationError | ItemNotInTemplate
[@@deriving show]
type configurationError = optionOrArgument * string * itemError
[@@deriving show]
let configurationError_to_string (kind, key, err) =
  let kindStr = match kind with Opt -> "Option" | Arg -> "Argument" in
  let errorMsg = match err with
    | Format validationErr -> Typeformat.validationError_to_string validationErr
    | ItemNotInTemplate -> "Item not in template"
  in
  Printf.sprintf "%s '%s': %s" kindStr key errorMsg
let validateConfiguration config template =
  let optionsErrors = StringMap.fold (fun k v accErrors->
      match StringMap.find_opt k template.optionsTemplate with
      | Some (Typeformat.Any optionTypeformat) -> let r = (Typeformat.validate_value optionTypeformat v) in if Result.is_ok r then accErrors else (Opt, k, Format (Result.get_error r))::accErrors
      | None -> (Opt, k, ItemNotInTemplate)::accErrors)
    config.Entry.options [] in
  let argsErrors = StringMap.fold (fun k v accErrors->
    match StringMap.find_opt k template.argsTemplate with
    | Some (Typeformat.Any argTypeformat) -> let r = (Typeformat.validate_value argTypeformat v) in if Result.is_ok r then accErrors else (Arg, k, Format (Result.get_error r))::accErrors
    | None -> (Arg, k, ItemNotInTemplate)::accErrors) 
    config.Entry.args [] in
  if optionsErrors = [] && argsErrors = [] then Result.Ok () else Result.Error (optionsErrors @ argsErrors)

