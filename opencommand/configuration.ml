open Command


type configurationTemplate = {
  optionsTemplate: Typeformat.typeformat Utils.StringMap.t;
  argsTemplate: Typeformat.typeformat Utils.StringMap.t;
}

let getConfigurationTemplate (head: head) : configurationTemplate =
  let optionsTemplate = head.options 
  |> Utils.StringMap.map (fun (opt) -> (
    match opt.CommandOption.encoding with
    | CommandOption.SingleString _ -> Typeformat.TUnit
    | CommandOption.ValueSuffix (_, enc) -> TokenEncoding.typeformat_of_tokenEncoding enc
    )) 
  in
  let argsTemplate = head.args
  |> Utils.StringMap.map (fun (arg) -> (
    match arg.Argument.encoding with
    | Argument.Single enc -> TokenEncoding.typeformat_of_tokenEncoding enc
    | Argument.ArgList (enc, _, _) -> Typeformat.TList (TokenEncoding.typeformat_of_tokenEncoding enc)
  )) in
  { optionsTemplate; argsTemplate }

let getEmptyConfiguration (template: configurationTemplate) : Entry.configuration =
  let options = Utils.StringMap.empty in
  let rec mapDefaultValue tf = match tf with 
      Typeformat.TUnit -> Typeformat.VUnit 
      | TString format_opt -> (match format_opt with
          Some Typeformat.QuotedString -> Typeformat.VString ""
          | Some Typeformat.SingleWord -> Typeformat.VString ""
          | Some Typeformat.FilePath -> Typeformat.VString ""
          | None -> Typeformat.VString "")
      | TInt -> Typeformat.VInt 0
      | TFloat -> Typeformat.VFloat 0.0
      | TEnum options -> Typeformat.VEnum (List.hd options)
      | TOption _ -> Typeformat.VOption None
      | TBool -> Typeformat.VBool false
      | TTuple types -> Typeformat.VTuple (List.map mapDefaultValue types)
      | TList _ -> Typeformat.VList []
  in
  let args = template.argsTemplate |> Utils.StringMap.map mapDefaultValue in
  { Entry.options; args }

type optionOrArgument = Opt | Arg
[@@deriving show]
let validateConfiguration config template =
  let optionsErrors = Utils.StringMap.fold (fun k v accErrors->
      match Utils.StringMap.find_opt k template.optionsTemplate with
      | Some optionTypeformat -> let r = (Typeformat.validate optionTypeformat v) in if Result.is_ok r then accErrors else (Opt, k, Result.get_error r)::accErrors
      | None -> (Opt, k, "Option not found in template")::accErrors)
    config.Entry.options [] in
  let argsErrors = Utils.StringMap.fold (fun k v accErrors->
    match Utils.StringMap.find_opt k template.argsTemplate with
    | Some argTypeformat -> let r = (Typeformat.validate argTypeformat v) in if Result.is_ok r then accErrors else (Arg, k, Result.get_error r)::accErrors
    | None -> (Arg, k, "Argument not found in template")::accErrors) 
    config.Entry.args [] in
  if optionsErrors = [] && argsErrors = [] then Result.Ok () else Result.Error (optionsErrors @ argsErrors)

