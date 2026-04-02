let getEmptyConfiguration (template: Command.configurationTemplate) : Entry.configuration =
  let options = Utils.StringMap.empty in
  let rec mapDefaultValue tf = match tf with 
      Typeformat.TUnit -> Typeformat.VUnit 
      | TString -> Typeformat.VString ""
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

let computeEntry (command:Command.command) (path:string) (configuration:Entry.configuration) : string =
  let head = Utils.StringMap.find path command.paths in
(*  let options = head.Command.options in
  let args = head.Command.args in *)
  let encodedPath = Command.EncodedPath.from_string path in
  let entryParts = List.map (
    fun token -> match token with
    | Command.Token.Directive s -> s
    | Command.Token.SingleArgumentLocation l | Command.Token.ArgumentLocation l -> 
        let argValue = Utils.StringMap.find_opt l configuration.Entry.args in
        (match argValue with
        | Some v -> Format.asprintf "%a" Typeformat.pp_value v
        | None -> Format.asprintf "%s" l)
    | Command.Token.OptionLocation l | Command.Token.SingleOptionLocation l ->
        let acceptableOptions = Entry.getOptionsFromLocation head l in
        let acceptedOptions = List.filter_map (fun (optName, opt) ->
          let optValue = Utils.StringMap.find_opt optName configuration.Entry.options in
          match optValue with
          | Some v -> (
          match opt.Command.CommandOption.encoding with
          | SingleString encodings -> Some (List.hd encodings)
          | ValueSuffix (suffixes, _valueFormat) -> 
              Some (List.hd suffixes ^ (Format.asprintf " %a" Typeformat.pp_value v)))
          | None -> None
        ) acceptableOptions in
        List.fold_left (fun acc opt -> acc ^ " " ^ opt) "" acceptedOptions
  ) encodedPath
  in
  List.fold_left (fun acc opt -> acc ^ " " ^ opt) "" entryParts