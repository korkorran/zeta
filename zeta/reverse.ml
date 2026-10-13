open Ocf
open Repr

type error = FeatureNotFound of string | ValidationError of (Configuration.configurationError) list
[@@deriving show]

let pathFromFeature (feature:string) (command:Command.command) : (FeaturePath.EncodedPath.t, error) Result.t =
  Result.Ok (StringMap.find feature command.features).Feature.path

(* writes one occurrence of an option *)
let writeOccurrence : type a. a Feature.CommandOption.written -> a -> string = fun written value ->
  match written with
  | Feature.CommandOption.SingleToken encoding -> Typeformat.token_of_value encoding value
  | Feature.CommandOption.ValueNextToken (keyEncoding, encoding) ->
    Typeformat.token_of_value keyEncoding () ^ " " ^ Typeformat.token_of_value encoding value

(* writes every occurrence of an option holding the configuration value [value] *)
let writeOption : type a. a Feature.CommandOption.encoding -> Typeformat.value -> string list = fun encoding (Typeformat.Value (tf, v)) ->
  match encoding with
  | Feature.CommandOption.Once written -> (
    match Typeformat.equal_type tf (Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding (Feature.CommandOption.valueEncoding written))) with
    | Some Type.Equal -> (match v with Some value -> [writeOccurrence written value] | None -> [])
    | None -> [])
  | Feature.CommandOption.Repeatable written -> (
    match Typeformat.equal_type tf (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding (Feature.CommandOption.valueEncoding written))) with
    | Some Type.Equal -> List.map (writeOccurrence written) v
    | None -> [])

let computeEntry (command:Command.command) (feature:string) (configuration:Configuration.configuration) : (string, error) Result.t =
  let result = StringMap.find_opt feature command.features in
  match result with
  | None -> Result.Error (FeatureNotFound feature)
  | Some (head) ->
  let validationResult = Configuration.validateConfiguration configuration (Configuration.getConfigurationTemplate head) in
  if Result.is_error validationResult then Result.Error (ValidationError (Result.get_error validationResult)) else
  let encodedPath = head.Feature.path in
  let entryParts = List.map (
    fun token -> match token with
    | FeaturePath.Token.Directive s -> s
    | FeaturePath.Token.ArgumentLocation l -> 
        let argValue = StringMap.find_opt l configuration.Configuration.args in
        (match argValue with
        | Some (Typeformat.Value (Typeformat.TOption _, None)) -> ""
        | Some v -> Format.asprintf "%a" Typeformat.pp_value v
        | None -> Format.asprintf "%s" l)
    | FeaturePath.Token.Anchor a ->
        let acceptableOptions = Option.getOptionsFromAnchor head a in
        let acceptedOptions = List.filter_map (fun (optName, Feature.CommandOption.Any opt) ->
          let optValue = StringMap.find_opt optName configuration.Configuration.options in
          match optValue with
          | Some value -> (
            match writeOption opt.encoding value with
            | [] -> None
            | occurrences -> Some (String.concat " " occurrences))
          | None -> None
        ) acceptableOptions in
        List.fold_left (fun acc opt -> if acc = "" then opt else acc ^ " " ^ opt) "" acceptedOptions
  ) encodedPath
  in
  Result.Ok (
    List.fold_left (fun acc part -> if acc = "" then part else if part = "" then acc else acc ^ " " ^ part) "" entryParts
  )