open Vdom

type model = {
  entry : string;
  tokens : string list option;
  selectedCommandKey : string option;
  selectedCommand : Mapping.Command.head Mapping.Command.StringMap.t option;
  results : (string * Mapping.Entry.result) list option;
  selectedPath : string option;
  configuration : Mapping.Entry.configuration option;
  configurationTemplate : Mapping.Command.configurationTemplate option;
}

let getSelectedConfiguration results =
  List.find_opt (fun (_path, result) -> match result with Mapping.Entry.Success _ -> true | _ -> false) results

let updateConfiguration model s selectedCommandKey =
    ignore model.selectedPath;
    ignore model.configuration;
    let entry = s in
    let tokens = Entry_tokenizer.Main.parse_with_error (Lexing.from_string entry) in
    let selectedCommand = Mapping.Command.StringMap.find_opt 
    (selectedCommandKey |> Option.value ~default:"git") (Native_registry.Registry.registry) 
    |> Option.value ~default:Native_registry.Git.git in
    let results = Mapping.Entry.evalConfiguration entry (selectedCommand) in
    (match getSelectedConfiguration results with
      | Some (selectedPath,Mapping.Entry.Success selectedConfiguration) -> 
        let selectedConfigurationTemplate = Mapping.Command.getConfigurationTemplate (Mapping.Command.StringMap.find selectedPath selectedCommand) in
        { 
          entry; 
          tokens; 
          selectedCommandKey = selectedCommandKey;
          selectedCommand = Some selectedCommand;
          results = Some results;
          selectedPath = Some selectedPath; 
          configuration = Some selectedConfiguration; 
          configurationTemplate = Some selectedConfigurationTemplate 
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
      })

let update model = function
  | `Text s -> updateConfiguration model s model.selectedCommandKey
  | `Unit -> model
  | `SelectCommand cmd -> updateConfiguration model model.entry (Some cmd)
  | `Click -> model

let init = { 
  entry = "git commit -m \"hello world\""; 
  tokens = None;
  selectedCommandKey = Some "mv";
  selectedCommand = None;
  results = None;
  selectedPath = None;
  configuration = None;
  configurationTemplate = None;  
}

let _button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select ?key  l = elt "select" ?key ~a:[onchange ~prevent_default:() ~stop_propagation:() (fun _ -> (`Unit)) ] l
let option ?(selected=false) txt =
  match selected with
  | true -> elt "option" ~a:[value txt; attr "selected" "selected"] [text txt]
  | _ -> elt "option" ~a:[value txt] [text txt]

let displayOption (k:string) (optionValue :Mapping.Typeformat.value option) (optionTypeformat:Mapping.Typeformat.typeformat) (desc:string) =
  let displayValue = 
    match optionTypeformat with
    | Mapping.Typeformat.TUnit -> text "()"
    | Mapping.Typeformat.TEnum enums ->
        select (List.map (fun e -> 
          match optionValue with 
          Some VEnum v -> if e = v
            then option ~selected:true e 
            else option e
          | _ -> option e) 
          enums)
    | Mapping.Typeformat.TString -> (match optionValue with 
      | Some VString s -> input [] ~a:[value s; oninput (fun _s -> `Unit)] 
      | _ -> input [] ~a:[value ""; oninput (fun _s -> `Unit)])
    | _ -> text "OTHERS" in
  div [ 
        div ~a:[class_ "checkbox-wrapper"; style "display" "inline-flex"; style "position" "relative"] [
          input [] ~a:[str_prop "type" "checkbox";
              bool_prop "checked" (match optionValue with Some _ -> true | _ -> false);
              onclick (fun _ -> (`Unit));
              class_ "checkbox"
             ]];
        div ~a:[class_ "tooltip"] [text (k^": "); div ~a:[class_ "tooltiptext"] [text desc]];
        displayValue
  ]



let displayConfiguration 
  (path : string option) 
  (config:Mapping.Entry.configuration option) 
  (configurationTemplate:Mapping.Command.configurationTemplate option) 
  (selectedCommand:Mapping.Command.head Mapping.Command.StringMap.t option) 
  =
  let open Mapping.Entry in
  match path, config, configurationTemplate, selectedCommand with
  | Some path, Some config, Some configurationTemplate, Some selectedCommand ->
  let args = StringMap.bindings config.args in
  let head = StringMap.find path selectedCommand in
  let optionsTemplate = configurationTemplate.optionsTemplate in
  let optionsBindings = StringMap.bindings optionsTemplate in
  div ~a:[style "border-style" "solid"; style "border-width" "1px"; style "width" "fit-content"; style "padding" "10px"] [
    text "Options:";
    if optionsBindings = [] then div [text "No options"] ~a:[style "font-style" "italic"; style "color" "gray"]
    else
    div (List.map (fun (k, optionTypeformat) -> 
      let optionValue = StringMap.find_opt k config.options in
      let optionDescription = (StringMap.find k head.options).Mapping.Command.CommandOption.description in
      displayOption k optionValue optionTypeformat optionDescription
    ) optionsBindings);
    text "Arguments:";
    if args = [] then div [text "No arguments"] ~a:[style "font-style" "italic"; style "color" "gray"]
    else
    div (List.map (fun (k, v) -> div [text (Format.asprintf "%s: %a" k Mapping.Typeformat.pp_value v)]) args);
  ]
  | _ -> div [text "No configuration to display"] ~a:[style "font-style" "italic"; style "color" "gray"]

  let displayPathResult command _configurationTemplate (path, result) =
    let getPathDescription path =
      match Mapping.Command.StringMap.find_opt path command with
      | Some head -> head.Mapping.Command.description
      | None -> "Unknown path"
    in
    let (desc, conf) = (match result with
      | Mapping.Entry.Success c -> ("Success", Some c)
      | Mapping.Entry.Unmatched t -> Printf.sprintf "Unmatched token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Incomplete t -> Printf.sprintf "Incomplete token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Overflow s -> Printf.sprintf "Overflow: %s" s,None) 
    in
    div [
      div [text path; text " -> "; text desc];
      div (match conf with Some _c -> [text (getPathDescription path); (* displayConfiguration path c configurationTemplate command *)] | None -> [])
    ]

let displaySelectableCommands (selectedCommand:string option) =
  let commands = Native_registry.Registry.registry |> Mapping.Command.StringMap.bindings in
  div ~a:[style "border-style" "solid"; style "border-width" "1px"; style "width" "fit-content"; style "padding" "10px"] [
    text "Available commands:";
    div (List.map (fun (k, _head) -> div [elt "a" ~a:[
      onclick (fun _ -> `SelectCommand k);
      style "cursor" "pointer";
      style "text-decoration" (match selectedCommand with Some sel when sel = k -> "underline" | _ -> "none")
    ] [text k]]) commands)
  ]

let view { entry;selectedCommandKey;tokens;results;selectedCommand;selectedPath;configurationTemplate;configuration } =
  let firstCol = div [text "Enter a command:"; displaySelectableCommands selectedCommandKey] 
  in
  let secondCol = div ((input [] ~a:[oninput (fun s -> `Text s); value entry; int_prop "size" 100])
    :: (div [text "Tokens: "; text (match tokens with Some ts -> String.concat ", " ts | None -> "None")])
    :: (match selectedCommand, results, configurationTemplate with
      | None, _, _ -> [div [text "No command selected"] ~a:[style "font-style" "italic"; style "color" "gray"]]
      | Some _, None,_ -> [div [text "No results"] ~a:[style "font-style" "italic"; style "color" "gray"]]
      | Some _, Some _, None -> [div [text "No configuration template"] ~a:[style "font-style" "italic"; style "color" "gray"]]
      | Some selectedCommand, Some results, Some template -> (results |> List.map (displayPathResult selectedCommand template))))
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
  let thirdCol = displayConfiguration selectedPath configuration configurationTemplate selectedCommand
  in
  div (elt "h1" [text "OpenCommand UI"] 
    :: div [ firstCol; secondCol; thirdCol  ] ~a:[style "display" "flex"; style "gap" "20px"]
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
let () = print_endline "hello world"; Window.set_onload window run