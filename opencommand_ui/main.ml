open Vdom

type optionOrArgument = Opt | Arg
type confValidError = (optionOrArgument * string * string) (*kind, key, message*)

type model = {
  entry : string;
  tokens : string list option;
  selectedCommandKey : string option;
  selectedCommand : Opencommand.Command.command option;
  results : (string * Opencommand.Entry.result) list option;
  selectedPath : string option;
  configuration : Opencommand.Entry.configuration option;
  configurationTemplate : Opencommand.Command.configurationTemplate option;
  configurationValid : (unit, confValidError list) Result.t;
}

let getSelectedConfiguration results =
  List.find_opt (fun (_path, result) -> match result with Opencommand.Entry.Success _ -> true | _ -> false) results

let validateConfiguration configuration configurationTemplate =
  match configuration, configurationTemplate with
  | Some config, Some template -> 
    let optionsErrors = Opencommand.Utils.StringMap.fold (fun k v accErrors->
        match Opencommand.Utils.StringMap.find_opt k template.Opencommand.Command.optionsTemplate with
        | Some optionTypeformat -> let r = (Opencommand.Typeformat.validate optionTypeformat v) in if Result.is_ok r then accErrors else (Opt, k, Result.get_error r)::accErrors
        | None -> (Opt, k, "Option not found in template")::accErrors)
      config.Opencommand.Entry.options [] in
    let argsErrors = Opencommand.Utils.StringMap.fold (fun k v accErrors->
      match Opencommand.Utils.StringMap.find_opt k template.Opencommand.Command.argsTemplate with
      | Some argTypeformat -> let r = (Opencommand.Typeformat.validate argTypeformat v) in if Result.is_ok r then accErrors else (Arg, k, Result.get_error r)::accErrors
      | None -> (Arg, k, "Argument not found in template")::accErrors) 
      config.Opencommand.Entry.args [] in
    if optionsErrors = [] && argsErrors = [] then Result.Ok () else Result.Error (optionsErrors @ argsErrors)
  | _ -> Result.Ok () (* If we don't have enough information to validate, we consider it valid for now *)

let optionOrArgumentValid configurationValid kind k =
  match configurationValid with
  | Result.Ok () -> Result.Ok ()
  | Result.Error errors -> match (List.find_opt (fun (errKind, errKey, _) -> errKind = kind && errKey = k) errors) with
    | Some (_, _, msg) -> Result.Error msg
    | None -> Result.Ok ()

let updateConfiguration s selectedCommandKey =
    let entry = s in
    let tokens = Entry_tokenizer.Main.parse_with_error (Lexing.from_string entry) in
    let selectedCommand = Opencommand.Utils.StringMap.find_opt 
    (selectedCommandKey |> Option.value ~default:"git") (Native_registry.Registry.registry) 
    |> Option.value ~default:Native_registry.Git.git in
    let results = Opencommand.Entry.evalConfiguration entry (selectedCommand.paths) in
    (match getSelectedConfiguration results with
      | Some (selectedPath,Opencommand.Entry.Success selectedConfiguration) -> 
        let selectedConfigurationTemplate = Opencommand.Command.getConfigurationTemplate (Opencommand.Utils.StringMap.find selectedPath selectedCommand.paths) in
        { 
          entry; 
          tokens; 
          selectedCommandKey = selectedCommandKey;
          selectedCommand = Some selectedCommand;
          results = Some results;
          selectedPath = Some selectedPath; 
          configuration = Some selectedConfiguration; 
          configurationTemplate = Some selectedConfigurationTemplate ;
          configurationValid = Ok ()
        }
      | _ -> { 
         entry; 
        tokens; 
        selectedCommandKey = selectedCommandKey;
        selectedCommand = Some selectedCommand;
        results = Some results;
        selectedPath = None;
        configuration = None;
        configurationTemplate = None;
        configurationValid = Ok ();
      })

let updateCommandPath model path =
  let template = (
      match model.selectedCommand with 
      Some cmd -> Some (Opencommand.Command.getConfigurationTemplate (Opencommand.Utils.StringMap.find path cmd.paths)) 
      | None -> None) in
  { 
    model with selectedPath = Some path; 
    configurationTemplate = template;
    configuration = (match template with Some t -> Some (Opencommand.Reverse.getEmptyConfiguration t) | None -> None);
  }

type updateKey = Whole of string | ListItem of string * int (*the list item and its index*)

let update model = function
  | `Text s -> updateConfiguration s model.selectedCommandKey
  | `Unit -> model
  | `SelectCommand cmd -> updateConfiguration model.entry (Some cmd)
  | `Click -> model
  | `Activate (on, k, v) -> let newConfig = match model.configuration with
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
    let entry = if Result.is_ok configurationValid then
     Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedPath) (Option.get newConfig) 
    else model.entry in
    { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | `UpdateConf (opt_or_arg, k, v) -> let newConfig = match model.configuration with
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
    let entry = if Result.is_ok configurationValid then
      Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedPath) (Option.get newConfig)
    else model.entry in
    { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | `AddListItem (opt_or_arg, k) -> let newConfig = match model.configuration with
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
    let entry = if Result.is_ok configurationValid then
      Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedPath) (Option.get newConfig)
    else model.entry in
    { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | `DeleteListItem (opt_or_arg, k, index) -> let newConfig = match model.configuration with
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
    let entry = if Result.is_ok configurationValid then
      Opencommand.Reverse.computeEntry (Option.get model.selectedCommand) (Option.get model.selectedPath) (Option.get newConfig)
    else model.entry in
    { model with configuration = newConfig; entry = entry; configurationValid = configurationValid }
  | `SelectPath path -> updateCommandPath model path

let init = { 
  entry = "mv aze rty uiop"; 
  tokens = None;
  selectedCommandKey = None;
  selectedCommand = None;
  results = None;
  selectedPath = None;
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

let displayOptionOrArg (kind:optionOrArgument) (k:string) (itemValue :Opencommand.Typeformat.value option) (itemTypeformat:Opencommand.Typeformat.typeformat) (desc:string) (optionOrArgumentValid:(unit, string) Result.t) =
  let activated = match itemValue with Some _ -> true | None -> false in
  let rec displayValue typeformat itemVal updateKey= 
    match typeformat with
    | Opencommand.Typeformat.TUnit -> text "()"
    | Opencommand.Typeformat.TEnum enums ->
        select ~a:[onchange ~prevent_default:() (fun x -> (`UpdateConf (kind, updateKey, Opencommand.Typeformat.VEnum x))) ] (List.map (fun e -> 
          match itemVal with 
          Some Opencommand.Typeformat.VEnum v -> if e = v
            then option ~selected:true e 
            else option e
          | _ -> option e) 
          enums)
    | Opencommand.Typeformat.TString _ -> (match itemVal with 
      | Some (Opencommand.Typeformat.VString s) -> input [] ~a:[value s; oninput (fun newValue -> `UpdateConf (kind, updateKey, Opencommand.Typeformat.VString newValue))] 
      | _ -> input [] ~a:[value ""; oninput (fun newValue -> `UpdateConf (kind, updateKey, Opencommand.Typeformat.VString newValue))])
    | Opencommand.Typeformat.TList tf -> (
       match itemVal with
       | Some (Opencommand.Typeformat.VList vs) -> div ((List.mapi (fun index v -> 
          let nestedUpdateKey = match updateKey with 
          | Whole key -> ListItem (key, index)
          | ListItem (_, _) -> failwith "Cannot nest list items at depth 2"
          in div [displayValue tf (Some v) nestedUpdateKey; button "×" (`DeleteListItem (kind, k, index))]) vs)
          @ [button "+" (`AddListItem (kind, k))])
       | _ -> text "List is empty"
    )
    | _ -> text "OTHERS" 
  in
    let checkbox = div ~a:[class_ "checkbox-wrapper"; style "display" "inline-flex"; style "position" "relative"] [
          input [] ~a:[str_prop "type" "checkbox";
              bool_prop "checked" activated;
              onclick (fun _ -> (`Activate (not activated, k, match itemValue with Some v -> v | None -> Opencommand.Typeformat.VUnit)));
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
      | Result.Error msg -> display @ [div ~a:[style "color" "red"; style "font-size" "0.8em"] [text msg]]
    in
    div ~a:[class_ "option-or-arg"] display



let displayConfiguration 
  (path : string option) 
  (config:Opencommand.Entry.configuration option) 
  (configurationTemplate:Opencommand.Command.configurationTemplate option) 
  (configurationValid:(unit, confValidError list) Result.t)
  (selectedCommand:Opencommand.Command.command option) 
  =
  let open Opencommand.Entry in
  match path, config, configurationTemplate, selectedCommand with
  | Some path, Some config, Some configurationTemplate, Some selectedCommand ->
  let args = Opencommand.Utils.StringMap.bindings config.args in
  let head = Opencommand.Utils.StringMap.find path selectedCommand.paths in
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

  let displayPathResult (command:Opencommand.Command.command) selectedPath (path, result) =
    let isPathSuccessfull = match result with Opencommand.Entry.Success _ -> true | _ -> false in
    let getPathDescription path =
      match Opencommand.Utils.StringMap.find_opt path command.paths with
      | Some head -> head.Opencommand.Command.description
      | None -> "Unknown path"
    in
    let (desc, _conf) = (match result with
      | Opencommand.Entry.Success c -> ("Success", Some c)
      | Opencommand.Entry.Unmatched t -> Printf.sprintf "Unmatched token: %s" (Opencommand.Command.Token.token_to_string t), None
      | Opencommand.Entry.Incomplete t -> Printf.sprintf "Incomplete token: %s" (Opencommand.Command.Token.token_to_string t), None
      | Opencommand.Entry.Overflow s -> Printf.sprintf "Overflow: %s" s,None) 
    in
    div ~a:[
      class_ (if selectedPath = path then "path-result selected" else "path-result");
      onclick (fun _ -> (print_endline "select path"; `SelectPath path));
      ] [
      div [text path; text " -> "; elt "span" ~a:[class_ (if isPathSuccessfull then "success" else "")] [text desc]];
      div  ~a:[class_ "description"] (if selectedPath = path then [text (getPathDescription path)] else [])
    ]

let displaySelectableCommands (selectedCommand:string option) =
  let commands = Native_registry.Registry.registry |> Opencommand.Utils.StringMap.bindings in
  div [
    text "Available commands:";
    div (List.map (fun (k, _head) -> div [elt "a" ~a:[
      onclick (fun _ -> `SelectCommand k);
      style "cursor" "pointer";
      style "text-decoration" (match selectedCommand with Some sel when sel = k -> "underline" | _ -> "none")
    ] [text k]]) commands)
  ]

let view { entry;selectedCommandKey;tokens;results;selectedCommand;selectedPath;configurationTemplate;configuration;configurationValid } =
  let firstCol = div [displaySelectableCommands selectedCommandKey] ~a:[class_ "commands"]
  in
  let secondCol = div ((input [] ~a:[oninput (fun s -> `Text s); value entry; int_prop "size" 100])
    :: (div [text "Tokens: "; text (match tokens with Some ts -> String.concat ", " ts | None -> "None")])
    :: (match selectedCommand, results with
      | None, _-> [div [text "No command selected"] ~a:[style "font-style" "italic"; style "color" "gray"]]
      | Some _, None-> [div [text "No results"] ~a:[style "font-style" "italic"; style "color" "gray"]]
      | Some selectedCommand, Some results -> 
        let sp = match selectedPath with Some sp -> sp | None -> "" in
        (results |> List.map (displayPathResult selectedCommand sp))))
  in
(*   let n = 3 in
  let _thirdCol = div
    [
      text (string_of_int n);
      svg_elt "svg"
        ~a:[
          int_attr "width" (200);
          int_attr "height" (200);
        ]
        [
          svg_elt "circle" []
            ~a:[
              onclick (fun _ -> `Click);
              int_attr "cx" (n * 10);
              int_attr "cy" (n * 10);
              int_attr "r" (n * 10);
              attr "fill" (if n mod 2 = 0 then "green" else "blue");
            ];
          svg_elt "path" []
            ~a:[
              attr "d" "M 40 40 A 70 70 0 0 1 40 140 L 60 140 A 70 70 0 0 0 60 40";
              attr "fill" "none";
              attr "stroke" "red";
              int_attr "stroke-width" (5);
            ] 
        ]
    ]*)
(*             <svg
  width="200"
  height="200"
  viewBox="-100 -100 200 200"
>
  <path
    d="
      M -40 -40
      A 70 70 0 0 0 40 40"
    fill="none"
    stroke="red"
    stroke-width="5"
  />
</svg> *)
  let thirdCol = displayConfiguration selectedPath configuration configurationTemplate configurationValid selectedCommand
  in
  let icon = elt "img" ~a:[attr "src" "logo.svg"; style "width" "32px"; style "height" "32px"] []
    
    
    (*  svg_elt "svg"
        ~a:[
          int_attr "width" (200);
          int_attr "height" (200);
          attr "viewBox" "0 0 16 16";
          int_attr "stroke-width" (0);
        ]
        [
          svg_elt "path" []
            ~a:[
              attr "d" "m8 0 6.61 3h.89a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.5.5H15v7a.5.5 0 0 1 .485.38l.5 2a.498.498 0 0 1-.485.62H.5a.498.498 0 0 1-.485-.62l.5-2A.5.5 0 0 1 1 13V6H.5a.5.5 0 0 1-.5-.5v-2A.5.5 0 0 1 .5 3h.89zM3.777 3h8.447L8 1zM2 6v7h1V6zm2 0v7h2.5V6zm3.5 0v7h1V6zm2 0v7H12V6zM13 6v7h1V6zm2-1V4H1v1zm-.39 9H1.39l-.25 1h13.72z";
              attr "fill" "none";
              attr "stroke" "red";
              int_attr "stroke-width" (5);
            ] 
        ] *)
  in
  div ( (div ~a:[class_ "header"] [elt "h1" [icon; text "Commandpedia"] ])
    :: div [ firstCol; secondCol; thirdCol  ] ~a:[class_ "columns"]
    :: [])
      
(*  :: (input [] ~a:[str_prop "type" "checkbox";
              bool_prop "checked" (checked);
              onclick (fun _ -> (`Check (not checked)));
             ];)
    :: (input [] ~a:[str_prop "type" "number"])
    :: (div [div [text "Checked!"]; text "Not checked"]) 
    :: (select [option "foo"; option "bar"]) *)      


let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = print_endline ("hello world:"^(Location.pathname (Window.location window))); Window.set_onload window run