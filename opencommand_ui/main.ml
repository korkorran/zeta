(* This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                                                    *)
(* Copyright (C) 2000-2024 LexiFi                                                                    *)

(* Inspired from https://github.com/janestreet/incr_dom/blob/master/example/incr_decr/counters.ml *)

open Vdom

module IntMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type model = {
  counters : int IntMap.t;
  entry : string;
  checked : bool;
}

let update { counters;entry;checked } = function
  | `New_counter -> { counters = IntMap.add (IntMap.cardinal counters) 0 counters;entry;checked }
  | `Update (pos, diff) -> { counters = IntMap.add pos (IntMap.find pos counters + diff) counters;entry;checked }
  | `Text s -> { counters;entry = s;checked }
  | `Check b -> { counters;entry;checked = b }
  | `Unit -> { counters;entry;checked }

let init = { counters = IntMap.empty; entry = "git merge --strategy octopus branch";checked = true }

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select ?key  l = elt "select" ?key ~a:[onchange ~prevent_default:() ~stop_propagation:() (fun _ -> (`Unit)) ] l
let option ?(selected=false) txt =
  match selected with
  | true -> elt "option" ~a:[value txt; attr "selected" "selected"] [text txt]
  | _ -> elt "option" ~a:[value txt] [text txt]

let displayOption (k,optionValue : string * Mapping.Typeformat.value) (optionsTypeformat:Mapping.Typeformat.typeformat Mapping.Command.StringMap.t) =
  let optionTypeformat = Mapping.Command.StringMap.find k optionsTypeformat in
  match optionTypeformat with
  | Mapping.Typeformat.TUnit -> text "UNIT"
  | Mapping.Typeformat.TEnum enums ->
    select (List.map (fun e -> 
      if Mapping.Typeformat.VEnum e = optionValue 
        then option ~selected:true e 
        else option e) 
      enums)
  | _ -> text "OTHERS"


let displayConfiguration path (config:Mapping.Entry.configuration) =
  let open Mapping.Entry in
  let options = StringMap.bindings config.options in
  let args = StringMap.bindings config.args in
  let optionsTypeformat = Mapping.Command.getOptionConfigurationTypeformat (Native_registry.Git.git) path in
  div ~a:[style "border-style" "solid"; style "border-width" "1px"; style "width" "fit-content"; style "padding" "10px"] [
    text "Options:";
    div (List.map (fun (k, v) -> displayOption (k,v) optionsTypeformat) options);
    text "Arguments:";
    div (List.map (fun (k, v) -> text (Format.asprintf "%s: %a" k Mapping.Typeformat.pp_value v)) args);
  ]

let view { entry;checked;_ } =
  let tokens = Entry_tokenizer.Main.parse_with_error (Lexing.from_string entry) in
  let results = Mapping.Entry.evalConfiguration entry (Native_registry.Git.git) in
  let getPathDescription path =
    match Mapping.Command.StringMap.find_opt path (Native_registry.Git.git) with
    | Some head -> head.description
    | None -> "Unknown path"
  in
  let row (path, result) =
    let (desc, conf) = (match result with
      | Mapping.Entry.Success c -> ("Success", Some c)
      | Mapping.Entry.Unmatched t -> Printf.sprintf "Unmatched token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Incomplete t -> Printf.sprintf "Incomplete token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Overflow s -> Printf.sprintf "Overflow: %s" s,None) in
    div [
      div [text path; text " -> "; text desc];
      div (match conf with Some c -> [text (getPathDescription path);displayConfiguration path c] | None -> [])
    ]
  in
  div (div [button "New counter" `New_counter] 
    :: (input [] ~a:[oninput (fun s -> `Text s); value entry; int_prop "size" 100])
    :: (input [] ~a:[str_prop "type" "checkbox";
              bool_prop "checked" (checked);
              onclick (fun _ -> (`Check (not checked)));
             ];)
    :: (input [] ~a:[str_prop "type" "number"])
    :: (div [div [text "Checked!"]; text "Not checked"]) 
    :: (select [option "foo"; option "bar"])
    :: (div [text "Tokens: "; text (match tokens with Some ts -> String.concat ", " ts | None -> "None")])
    :: (results |> List.map row))


let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = print_endline "hello world"; Window.set_onload window run