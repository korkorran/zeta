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
  let withOption typeformat value = {Configuration.options=StringMap.add id (Typeformat.Value (Typeformat.TOption typeformat, Some value)) currentConfig.options; args=currentConfig.args} in
  match option.encoding with
  | Feature.CommandOption.SingleToken encoding -> (
    match restEntry with
    | he::te -> (
      match Typeformat.value_of_token encoding he with
      | Some value -> Some (withOption (Typeformat.typeformat_of_tokenEncoding encoding) value, [he], te)
      | None -> None)
    | [] -> None)
  | Feature.CommandOption.ValueNextToken (keyEncoding, encoding) -> (
    match restEntry with
    | h1::h2::te -> (
      match Typeformat.value_of_token keyEncoding h1, Typeformat.value_of_token encoding h2 with
      | Some (), Some value -> Some (withOption (Typeformat.typeformat_of_tokenEncoding encoding) value, [h1; h2], te)
      | _ -> None)
    | _ -> None)
