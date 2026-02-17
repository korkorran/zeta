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

let init = { counters = IntMap.empty; entry = "git config --list";checked = true }

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select ?key ?a l = elt "select" ?key ?a l
let option txt = elt "option" ~a:[value txt] [text txt]

let displayConfiguration (config:Mapping.Entry.configuration) =
  let open Mapping.Entry in
  let options = StringMap.bindings config.options in
  let args = StringMap.bindings config.args in
  div [
    text "Options:";
    div (List.map (fun (k, v) -> text (Format.asprintf "%s: %a" k Mapping.Typeformat.pp_value v)) options);
    text "Arguments:";
    div (List.map (fun (k, v) -> text (Format.asprintf "%s: %a" k Mapping.Typeformat.pp_value v)) args);
  ]

let view { entry;checked;_ } =
  let tokens = Entry_tokenizer.Main.parse_with_error (Lexing.from_string entry) in
  let results = Mapping.Entry.evalConfiguration entry (Native_registry.Git.git) in
  let row (path, result) =
    let (desc, conf) = (match result with
      | Mapping.Entry.Success c -> ("Success", Some c)
      | Mapping.Entry.Unmatched t -> Printf.sprintf "Unmatched token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Incomplete t -> Printf.sprintf "Incomplete token: %s" (Mapping.Command.Token.token_to_string t), None
      | Mapping.Entry.Overflow s -> Printf.sprintf "Overflow: %s" s,None) in
    div [
      div [text path; text " -> "; text desc];
      div [(match conf with Some c -> displayConfiguration c | None -> div [])]
    ]
  in
  div (div [button "New counter" `New_counter] 
    :: (input [] ~a:[oninput (fun s -> `Text s); value entry])
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