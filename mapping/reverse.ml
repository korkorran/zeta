let computeEntry (command:Command.rawCommand) (path:string) (configuration:Entry.configuration) : string =
  ignore command;
  ignore path;
  ignore configuration;
  let head = Command.StringMap.find path command in
(*  let options = head.Command.options in
  let args = head.Command.args in *)
  let encodedPath = Command.EncodedPath.from_string path in
  let entryParts = List.map (
    fun token -> match token with
    | Command.Token.Directive s -> s
    | Command.Token.SingleArgumentLocation l | Command.Token.ArgumentLocation l -> 
        let argValue = Command.StringMap.find_opt l configuration.Entry.args in
        (match argValue with
        | Some v -> Format.asprintf "{%a}" Typeformat.pp_value v
        | None -> Format.asprintf "{%s}" l)
    | Command.Token.OptionLocation l | Command.Token.SingleOptionLocation l ->
        let acceptableOptions = Entry.getOptionsFromLocation head l in
        let acceptedOptions = List.filter_map (fun (optName, opt) ->
          let optValue = Command.StringMap.find_opt optName configuration.Entry.options in
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