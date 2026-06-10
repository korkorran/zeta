
open Vdom


module type Injectable = sig
  type 'msg upper_msg
  val wrapMessage : 'msg -> 'msg upper_msg
  val initCommandCallback : string -> 'msg upper_msg
end

module type S = sig
  type 'msg upper_msg
  type model
  type msg
  val init : model
  val update : model -> msg -> model * (msg upper_msg Cmd.t list)
  val view : model -> msg upper_msg vdom
end

module Make(Injectable: Injectable) : S with type 'msg upper_msg := 'msg Injectable.upper_msg = struct

type updateKey = Whole of string | ListItem of string * int (*the list item and its index*)

type msg =
  | Text of string
  | Run
  | SelectCommand of string
  | Activate of bool * string * Opencommand.Typeformat.value (* on/off, key, value *)
  | UpdateConf of Opencommand.Configuration.optionOrArgument * updateKey * Opencommand.Typeformat.value (* kind, key or list item, new value *)
  | AddListItem of Opencommand.Configuration.optionOrArgument * string (* kind, key *)
  | DeleteListItem of Opencommand.Configuration.optionOrArgument * string * int (* kind, key, index *)
  | SelectFeature of string

type model = {
  entry : string;
  tokens : string list option;
  selectedCommandKey : string option;
  selectedCommand : Opencommand.Command.command option;
  results : (string * Opencommand.Entry.result) list option;
  selectedFeature : string option;
  configuration : Opencommand.Entry.configuration option;
  configurationTemplate : Opencommand.Configuration.configurationTemplate option;
  configurationValid : (unit, Opencommand.Configuration.configurationError list) Result.t;
}


let getSelectedConfiguration results =
  List.find_opt (fun (_path, result) -> match result with Opencommand.Entry.Success _ -> true | _ -> false) results

let validateConfiguration configuration configurationTemplate =
  match configuration, configurationTemplate with
  | Some config, Some template -> 
    Opencommand.Configuration.validateConfiguration config template
  | _ -> Result.Ok () (* If we don't have enough information to validate, we consider it valid for now *)

let optionOrArgumentValid configurationValid kind k =
  match configurationValid with
  | Result.Ok () -> Result.Ok ()
  | Result.Error errors -> match (List.find_opt (fun (errKind, errKey, _) -> errKind = kind && errKey = k) errors) with
    | Some err -> Result.Error err
    | None -> Result.Ok ()

let updateConfiguration s selectedCommandKey =
    let entry = s in
    let tokens = Entry_tokenizer.Main.parse_with_error (Lexing.from_string entry) in
    let selectedCommand = Opencommand.Utils.StringMap.find_opt 
    (selectedCommandKey |> Option.value ~default:"git") (Native_registry.Registry.registry) 
    |> Option.value ~default:Native_registry.Git.git in
(*  Printf.printf "Selected command: %s\n" (match selectedCommandKey with Some k -> k | None -> "None"); *)
    let results = Opencommand.Entry.evalConfiguration entry (selectedCommand.features) in
    (match getSelectedConfiguration results with
      | Some (selectedFeatureId,Opencommand.Entry.Success selectedConfiguration) ->
        let head = (Opencommand.Utils.StringMap.find selectedFeatureId selectedCommand.features) in
        let selectedConfigurationTemplate = Opencommand.Configuration.getConfigurationTemplate head in
        { 
          entry; 
          tokens; 
          selectedCommandKey = selectedCommandKey;
          selectedCommand = Some selectedCommand;
          results = Some results;
          selectedFeature = Some selectedFeatureId;
          configuration = Some selectedConfiguration; 
          configurationTemplate = Some selectedConfigurationTemplate ;
          configurationValid = Ok ();
        }
      | _ -> { 
         entry; 
        tokens; 
        selectedCommandKey = selectedCommandKey;
        selectedCommand = Some selectedCommand;
        results = Some results;
        selectedFeature = None;
        configuration = None;
        configurationTemplate = None;
        configurationValid = Ok ();
      })

let updateCommandFeature model feature =
  let template = 
      match model.selectedCommand with 
      Some cmd -> 
        (match Opencommand.Reverse.pathFromFeature feature cmd with
        | Result.Ok _path -> let head = Opencommand.Utils.StringMap.find feature cmd.features in
          Some (Opencommand.Configuration.getConfigurationTemplate head)
        | Result.Error _ -> None)
      | None -> None in
  { 
    model with selectedFeature = Some feature; 
    configurationTemplate = template;
    configuration = (match template with Some t -> Some (Opencommand.Configuration.getEmptyConfiguration t) | None -> None);
  }

let return ?(c=[]) x = (x, c)

let update model = function
  | Text s -> return @@ updateConfiguration s model.selectedCommandKey
  (* | Unit ->
  let expr = "window.electronAPI.ping" in
  let on_success = (fun r -> Click (model.entry, r)) in 
  let on_failure = (fun e -> Click (model.entry, e)) in
    return ~c:[Lib.Await {expr; on_success; on_failure}] model *)
  | Run -> 
      let on_success = (fun _r -> Injectable.initCommandCallback (model.entry)) in 
      let on_failure = (fun _e -> Injectable.initCommandCallback ("ERROR: " ^ model.entry)) in
      return ~c:[Lib.RunCommand {cmd = model.entry; on_success; on_failure}] model
  | SelectCommand cmd -> return @@ updateConfiguration model.entry (Some cmd)
  | Activate (on, k, v) -> let newConfig = match model.configuration with
    | Some config -> 
        let v = match model.configurationTemplate with
          | None -> v
          | Some template -> (match Opencommand.Utils.StringMap.find_opt k template.optionsTemplate with
            | Some optionTypeformat -> (match optionTypeformat with
              | Opencommand.Typeformat.TUnit -> Opencommand.Typeformat.VUnit
              | Opencommand.Typeformat.TEnum l -> (match v with Opencommand.Typeformat.VEnum s -> Opencommand.Typeformat.VEnum s | _ -> Opencommand.Typeformat.VEnum (List.hd l))
              | Opencommand.Typeformat.TString _ -> (match v with Opencommand.Typeformat.VString s -> Opencommand.Typeformat.VString s | _ -> v)
              | _ -> v)
            | None -> v) in
        let newOptions = if on then Opencommand.Utils.StringMap.add k v config.Opencommand.Entry.options else Opencommand.Utils.StringMap.remove k config.Opencommand.Entry.options in
        Some { config with Opencommand.Entry.options = newOptions }
    | None -> None
    in
    let configurationValid = validateConfiguration newConfig model.configurationTemplate in
    let computedEntryResult =
     Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedFeature) (Option.get newConfig) 
      in
    let entry = if Result.is_ok configurationValid && Result.is_ok computedEntryResult then
      Result.get_ok computedEntryResult
    else model.entry in
    return @@ { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | UpdateConf (opt_or_arg, k, v) -> let newConfig = match model.configuration with
    | Some config -> (match opt_or_arg with
      | Opt -> (match k with 
        | Whole key -> Some { config with Opencommand.Entry.options = Opencommand.Utils.StringMap.add key v config.Opencommand.Entry.options }
        | ListItem (key, index) -> let currentVal = Opencommand.Utils.StringMap.find_opt key config.Opencommand.Entry.options in
          (match currentVal with
          | Some (Opencommand.Typeformat.VList vs) -> 
              let newList = if index < List.length vs then List.mapi (fun i item -> if i = index then v else item) vs else vs @ [v] in
              Some { config with Opencommand.Entry.options = Opencommand.Utils.StringMap.add key (Opencommand.Typeformat.VList newList) config.Opencommand.Entry.options }
          | _ -> None))
      | Arg -> (match k with 
        | Whole key -> Some { config with Opencommand.Entry.args = Opencommand.Utils.StringMap.add key v config.Opencommand.Entry.args }
        | ListItem (key, index) -> let currentVal = Opencommand.Utils.StringMap.find_opt key config.Opencommand.Entry.args in
          (match currentVal with
          | Some (Opencommand.Typeformat.VList vs) -> 
              let newList = if index < List.length vs then List.mapi (fun i item -> if i = index then v else item) vs else vs @ [v] in
              Some { config with Opencommand.Entry.args = Opencommand.Utils.StringMap.add key (Opencommand.Typeformat.VList newList) config.Opencommand.Entry.args }
          | _ -> None))
    )
    | None -> None
    in
    let configurationValid = validateConfiguration newConfig model.configurationTemplate in
    let computedEntryResult =   Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedFeature) (Option.get newConfig)
    in
    let entry = if Result.is_ok configurationValid && Result.is_ok computedEntryResult then
      Result.get_ok computedEntryResult
    else model.entry in
    return @@ { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | AddListItem (opt_or_arg, k) -> let newConfig = match model.configuration with
    | Some config -> (match opt_or_arg with
      | Opt -> Some { config with Opencommand.Entry.options = Opencommand.Utils.StringMap.update k (function
        | Some (Opencommand.Typeformat.VList vs) -> Some (Opencommand.Typeformat.VList (vs @ [Opencommand.Typeformat.VUnit]))
        | _ -> Some (Opencommand.Typeformat.VList [Opencommand.Typeformat.VUnit])) config.Opencommand.Entry.options }
      | Arg -> Some { config with Opencommand.Entry.args = Opencommand.Utils.StringMap.update k (function
        | Some (Opencommand.Typeformat.VList vs) -> Some (Opencommand.Typeformat.VList (vs @ [Opencommand.Typeformat.VUnit]))
        | _ -> Some (Opencommand.Typeformat.VList [Opencommand.Typeformat.VUnit])) config.Opencommand.Entry.args }
    )
    | None -> None
    in
    let configurationValid = validateConfiguration newConfig model.configurationTemplate in
    let computedEntryResult =   Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedFeature) (Option.get newConfig)
    in
    let entry = if Result.is_ok configurationValid && Result.is_ok computedEntryResult then
      Result.get_ok computedEntryResult
    else model.entry in
    return @@ { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | DeleteListItem (opt_or_arg, k, index) -> let newConfig = match model.configuration with
    | Some config -> (match opt_or_arg with
      | Opt -> Some { config with Opencommand.Entry.options = Opencommand.Utils.StringMap.update k (function
        | Some (Opencommand.Typeformat.VList vs) -> Some (Opencommand.Typeformat.VList (List.filteri (fun i _ -> i <> index) vs))
        | _ -> None) config.Opencommand.Entry.options }
      | Arg -> Some { config with Opencommand.Entry.args = Opencommand.Utils.StringMap.update k (function
        | Some (Opencommand.Typeformat.VList vs) -> Some (Opencommand.Typeformat.VList (List.filteri (fun i _ -> i <> index) vs))
        | _ -> None) config.Opencommand.Entry.args }
    )
    | None -> None
    in
    let configurationValid = validateConfiguration newConfig model.configurationTemplate in
    let computedEntryResult = Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedFeature) (Option.get newConfig)
    in
    let entry = if Result.is_ok configurationValid && Result.is_ok computedEntryResult then
      Result.get_ok computedEntryResult
    else model.entry in
    return @@ { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | SelectFeature feature -> return @@ updateCommandFeature model feature

let init = { 
  entry = "ls -a"; 
  tokens = None;
  selectedCommandKey = None;
  selectedCommand = None;
  results = None;
  selectedFeature = None;
  configuration = None;
  configurationTemplate = None;  
  configurationValid = Result.Ok ();
}



let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select ?key ~a l = elt "select" ?key ~a l
let option ?(selected=false) txt =
  match selected with
  | true -> elt "option" ~a:[value txt; attr "selected" "selected"] [text txt]
  | _ -> elt "option" ~a:[value txt] [text txt]

let displayOptionOrArg 
  (kind:Opencommand.Configuration.optionOrArgument) 
  (k:string) 
  (itemValue :Opencommand.Typeformat.value option) 
  (itemTypeformat:Opencommand.Typeformat.typeformat) 
  (desc:string) 
  (optionOrArgumentValid:(unit, Opencommand.Configuration.configurationError) Result.t) 
  =
  let activated = match itemValue with Some _ -> true | None -> false in
  let rec displayValue typeformat itemVal updateKey= 
    match typeformat with
    | Opencommand.Typeformat.TUnit -> text "()"
    | Opencommand.Typeformat.TEnum enums ->
        select ~a:[onchange ~prevent_default:() (fun x -> (Injectable.wrapMessage @@ UpdateConf (kind, updateKey, Opencommand.Typeformat.VEnum x))) ] (List.map (fun e -> 
          match itemVal with 
          Some Opencommand.Typeformat.VEnum v -> if e = v
            then option ~selected:true e 
            else option e
          | _ -> option e) 
          enums)
    | Opencommand.Typeformat.TString _ -> (match itemVal with 
      | Some (Opencommand.Typeformat.VString s) -> input [] ~a:[value s; oninput (fun newValue -> Injectable.wrapMessage @@ UpdateConf (kind, updateKey, Opencommand.Typeformat.VString newValue))] 
      | _ -> input [] ~a:[value ""; oninput (fun newValue -> Injectable.wrapMessage @@ UpdateConf (kind, updateKey, Opencommand.Typeformat.VString newValue))])
    | Opencommand.Typeformat.TList tf -> (
       match itemVal with
       | Some (Opencommand.Typeformat.VList vs) -> div ((List.mapi (fun index v -> 
          let nestedUpdateKey = match updateKey with 
          | Whole key -> ListItem (key, index)
          | ListItem (_, _) -> failwith "Cannot nest list items at depth 2"
          in div [displayValue tf (Some v) nestedUpdateKey; button "×" (Injectable.wrapMessage @@ DeleteListItem (kind, k, index))]) vs)
          @ [button "+" (Injectable.wrapMessage @@ AddListItem (kind, k))])
       | _ -> text "List is empty"
    )
    | _ -> text "OTHERS" 
  in
    let checkbox = div ~a:[class_ "checkbox-wrapper"; style "display" "inline-flex"; style "position" "relative"] [
          input [] ~a:[str_prop "type" "checkbox";
              bool_prop "checked" activated;
              onclick (fun _ -> (Injectable.wrapMessage @@ Activate (not activated, k, match itemValue with Some v -> v | None -> Opencommand.Typeformat.VUnit)));
              class_ "checkbox"
             ]]
    in
    let display = [
        div ~a:[class_ "tooltip"] [text (k^": "); div ~a:[class_ "tooltiptext"] [text desc]];
        displayValue itemTypeformat itemValue (Whole k)
    ]
    in
    let display = match kind with
      | Opt -> checkbox :: display
      | Arg -> display
    in
    let display = match optionOrArgumentValid with
      | Result.Ok () -> display
      | Result.Error err -> display @ [div ~a:[style "color" "red"; style "font-size" "0.8em"] [text (Opencommand.Configuration.configurationError_to_string err)]]
    in
    div ~a:[class_ "option-or-arg"] display



let displayConfiguration 
  (feature : string option) 
  (config:Opencommand.Entry.configuration option) 
  (configurationTemplate:Opencommand.Configuration.configurationTemplate option) 
  (configurationValid:(unit, Opencommand.Configuration.configurationError list) Result.t)
  (selectedCommand:Opencommand.Command.command option) 
  =
  let open Opencommand.Entry in
  match feature, config, configurationTemplate, selectedCommand with
  | Some feature, Some config, Some configurationTemplate, Some selectedCommand ->
  let args = Opencommand.Utils.StringMap.bindings config.args in
  let head = Opencommand.Utils.StringMap.find feature selectedCommand.features in
  let optionsTemplate = configurationTemplate.optionsTemplate in
  let optionsBindings = Opencommand.Utils.StringMap.bindings optionsTemplate in
  div ~a:[class_ ("configuration" ^ (if Result.is_error configurationValid then " invalid" else ""))] [
    text "Options:";
    if optionsBindings = [] then div [text "No options"] ~a:[style "font-style" "italic"; style "color" "gray"]
    else
    div (List.map (fun (k, optionTypeformat) -> 
      let optionValue = Opencommand.Utils.StringMap.find_opt k config.options in
      let optionDescription = (Opencommand.Utils.StringMap.find k head.options).Opencommand.Command.CommandOption.description in
      displayOptionOrArg Opt k optionValue optionTypeformat optionDescription (optionOrArgumentValid configurationValid Opt k)
    ) optionsBindings);
    text "Arguments:";
    if args = [] then div [text "No arguments"] ~a:[style "font-style" "italic"; style "color" "gray"]
    else
    div (List.map 
    (fun (k, v) -> let argTypeformat = (Opencommand.Utils.StringMap.find k configurationTemplate.argsTemplate) in 
      let description = (Opencommand.Utils.StringMap.find k head.args).Opencommand.Command.Argument.description in
      displayOptionOrArg Arg k (Some v) argTypeformat description (optionOrArgumentValid configurationValid Arg k))
     args);
  ]
  | _ -> div [text "No configuration to display"] ~a:[style "font-style" "italic"; style "color" "gray"; class_ "configuration"]

  let displayPathResult (command:Opencommand.Command.command) selectedFeature (feature, result) =
    let isPathSuccessfull = match result with Opencommand.Entry.Success _ -> true | _ -> false in
    let getPathDescription () =
      match Opencommand.Utils.StringMap.find_opt feature command.features with
      | Some head -> head.Opencommand.Command.description
      | None -> "Unknown path"
    in
    let getPathFromFeature () =
      match Opencommand.Utils.StringMap.find_opt feature command.features with
      | Some head -> head.Opencommand.Command.path
      | None -> "Unknown path"
    in
    let (desc, _conf) = (match result with
      | Opencommand.Entry.Success c -> ("Success", Some c)
      | Opencommand.Entry.Unmatched t -> Printf.sprintf "Unmatched token: %s" (Opencommand.Command.Token.token_to_string t), None
      | Opencommand.Entry.Incomplete t -> Printf.sprintf "Incomplete token: %s" (Opencommand.Command.Token.token_to_string t), None
      | Opencommand.Entry.Overflow s -> Printf.sprintf "Overflow: %s" s,None) 
    in
    div ~a:[class_ (if selectedFeature = feature then "path-result selected" else "path-result")] [
      div ~a:[
        class_ "row";
        onclick (fun _ -> (print_endline "select feature"; Injectable.wrapMessage @@ SelectFeature feature));
        ] [
        div [text feature; text " -> "; elt "span" ~a:[class_ (if isPathSuccessfull then "success" else "")] [text desc]];
        div  ~a:[class_ "description"] (if selectedFeature = feature then [text (getPathDescription ())] else []);
        elt "br" [];
      ];
      div ~a:[class_ "path"] [text (getPathFromFeature ())];
     ]

let displaySelectableCommands (selectedCommand:string option) =
  let commands = Native_registry.Registry.registry |> Opencommand.Utils.StringMap.bindings in
  div [
    text "Available commands:";
    div (List.map (fun (k, _head) -> div [elt "a" ~a:[
      onclick (fun _ -> Injectable.wrapMessage @@ SelectCommand k);
      style "cursor" "pointer";
      style "text-decoration" (match selectedCommand with Some sel when sel = k -> "underline" | _ -> "none")
    ] [text k]]) commands)
  ]

let view ({ entry;selectedCommandKey;tokens;results;selectedCommand;selectedFeature;configurationTemplate;configuration;configurationValid; }: model)  =
  let firstCol = div [displaySelectableCommands selectedCommandKey] ~a:[class_ "commands"]
  in
  let secondCol = div (
    div ~a:[class_ "input-group"] [input [] ~a:[oninput (fun s -> Injectable.wrapMessage (Text s)); value entry; int_prop "size" 100] ; button "Run" (Injectable.wrapMessage Run)]
    :: (div [text "Tokens: "; text (match tokens with Some ts -> String.concat ", " ts | None -> "None")])
    :: [match selectedCommand, results with
      | None, _-> div [text "No command selected"] ~a:[style "font-style" "italic"; style "color" "gray"]
      | Some _, None-> div [text "No results"] ~a:[style "font-style" "italic"; style "color" "gray"]
      | Some selectedCommand, Some results -> 
        let sf = match selectedFeature with Some sf -> sf | None -> "" in
        div (results |> List.map (displayPathResult selectedCommand sf)) ~a:[class_ "paths-results"]]
    )
  in
  let thirdCol = displayConfiguration selectedFeature configuration configurationTemplate configurationValid selectedCommand
in div [ firstCol; secondCol; thirdCol  ] ~a:[class_ "columns"]  

end

