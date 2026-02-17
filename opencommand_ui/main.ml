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
  | `Text s -> { counters = IntMap.add 0 (int_of_string s) counters;entry = s;checked }
  | `Check b -> { counters;entry;checked = b }

let init = { counters = IntMap.empty; entry = "0";checked = true }

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let select ?key ?a l = elt "select" ?key ?a l
let option txt = elt "option" ~a:[value txt] [text txt]

let view { counters;entry;checked } =
  let row (pos, value) =
    div [button "-" (`Update (pos, -1)); text (string_of_int value); button "+" (`Update (pos, 1))]
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
    :: (IntMap.bindings counters |> List.map row))


let app = simple_app ~init ~view ~update ()


open Js_browser

let run () = Vdom_blit.run app |> Vdom_blit.dom |> Element.append_child (Document.body document)
let () = print_endline "hello world"; Window.set_onload window run