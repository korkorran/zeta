
type error = FeatureNotFound of string | ValidationError of (Configuration.configurationError) list
[@@deriving show]

let pathFromFeature (feature:string) (command:Command.command) : (string, error) Result.t =
  Result.Ok (StringMap.find feature command.features).Command.path

let computeEntry (command:Command.command) (feature:string) (configuration:Entry.configuration) : (string, error) Result.t =
  let result = StringMap.find_opt feature command.features in
  match result with
  | None -> Result.Error (FeatureNotFound feature)
  | Some (head) ->
  let validationResult = Configuration.validateConfiguration configuration (Configuration.getConfigurationTemplate head) in
  if Result.is_error validationResult then Result.Error (ValidationError (Result.get_error validationResult)) else
  let encodedPath = FeaturePath.EncodedPath.from_string head.Command.path in
  let entryParts = List.map (
    fun token -> match token with
    | FeaturePath.Token.Directive s -> s
    | FeaturePath.Token.SingleArgumentLocation l | FeaturePath.Token.ArgumentLocation l -> 
        let argValue = StringMap.find_opt l configuration.Entry.args in
        (match argValue with
        | Some v -> Format.asprintf "%a" Typeformat.pp_value v
        | None -> Format.asprintf "%s" l)
    | FeaturePath.Token.Anchor l | FeaturePath.Token.SingleOptionAnchor l ->
        let acceptableOptions = Entry.getOptionsFromLocation head l in
        let acceptedOptions = List.filter_map (fun (optName, opt) ->
          let optValue = StringMap.find_opt optName configuration.Entry.options in
          match optValue with
          | Some v -> (
          match opt.Command.CommandOption.encoding with
          | SingleString encodings -> Some (List.hd encodings)
          | ValueSuffix (suffixes, _valueFormat) -> 
              Some (List.hd suffixes ^ (Format.asprintf " %a" Typeformat.pp_value v)))
          | None -> None
        ) acceptableOptions in
        List.fold_left (fun acc opt -> if acc = "" then opt else acc ^ " " ^ opt) "" acceptedOptions
  ) encodedPath
  in
  Result.Ok (
    List.fold_left (fun acc part -> if acc = "" then part else if part = "" then acc else acc ^ " " ^ part) "" entryParts
  )