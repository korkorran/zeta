open Ocf
open Repr

type error = FeatureNotFound of string | ValidationError of (Configuration.configurationError) list
[@@deriving show]

let pathFromFeature (feature:string) (command:Command.command) : (FeaturePath.EncodedPath.t, error) Result.t =
  Result.Ok (StringMap.find feature command.features).Feature.path

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
          | Some (Typeformat.Value (Typeformat.TOption _, None)) -> None
          | Some (Typeformat.Value (tf, v)) -> (
          (* writes the option value with the token encoding [encoding] *)
          let token_of_option (type a) (encoding : a TokenEncoding.tokenEncoding) =
            match Typeformat.equal_type tf (Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding encoding)) with
            | Some Type.Equal -> Stdlib.Option.map (Typeformat.token_of_value encoding) v
            | None -> None
          in
          match opt.encoding with
          | Feature.CommandOption.SingleToken encoding -> token_of_option encoding
          | Feature.CommandOption.ValueNextToken (keyEncoding, encoding) ->
              Stdlib.Option.map (fun value -> Typeformat.token_of_value keyEncoding () ^ " " ^ value) (token_of_option encoding))
          | None -> None
        ) acceptableOptions in
        List.fold_left (fun acc opt -> if acc = "" then opt else acc ^ " " ^ opt) "" acceptedOptions
  ) encodedPath
  in
  Result.Ok (
    List.fold_left (fun acc part -> if acc = "" then part else if part = "" then acc else acc ^ " " ^ part) "" entryParts
  )