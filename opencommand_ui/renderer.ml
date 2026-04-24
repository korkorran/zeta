open Vdom

let () = ignore Lib.unused

type 'inner_msg msg =
  | CommandSelector of 'inner_msg
  | TerminalOutput of string
(*   | InitCommand of string *)

module CommandSelectorApp = CommandSelector.Make(
  struct 
    type 'msg upper_msg = 'msg msg 
    let wrapMessage x = CommandSelector x 
  end
)

type model = {
  commandSelector : CommandSelectorApp.model;
  responses : (string * string) list; (* For storing responses from commands *)
}

let rec appendOutputToResponses output responses =
  match responses with
  | [] -> failwith "No entry registered for terminal output"
  | [(k, v)] -> [(k, v ^ output)]
  | other::rest -> other :: appendOutputToResponses output rest

let update model = function
(*   | InitCommand command ->  (
      try let responses = model.responses @ [(command, "")] in return @@ { model with responses }
      with e -> print_endline (Printexc.to_string e); 
       return @@ model) *)
  | TerminalOutput output -> let responses = appendOutputToResponses output model.responses in return @@ { model with responses }
  | CommandSelector msg -> 
    let (updatedCommandSelector, cmds) = CommandSelectorApp.update model.commandSelector msg in 
    return ~c:(cmds) { model with commandSelector = updatedCommandSelector }

let init = return { 
  commandSelector = CommandSelectorApp.init;
  responses = [];
}

let view { commandSelector; responses } =
  let icon = elt "img" ~a:[attr "src" "opencommand_ui/logo.svg"; style "width" "32px"; style "height" "32px"] []
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
  let displayResponses = div ~a:[class_ "responses"] (List.map (fun (_k, v) -> elt "pre" ~a:[class_ "response"] [text ((* k ^ ": " ^ *) v)]) responses) in
  div ( (div ~a:[class_ "header"] [elt "h1" [icon; text "Commandpedia"];
  elt "p" [text (Lib.electron_version ^ " version - Desktop mode: " ^ (string_of_bool Lib.is_desktop_mode))]])
    :: (CommandSelectorApp.view commandSelector)
    :: displayResponses
    :: [])  


let app = app ~init ~view ~update ()


open Js_browser


let run () = 
  let vdom_blit_app =Vdom_blit.run app in 
  vdom_blit_app |> Vdom_blit.dom |> Element.append_child (Document.body document);
  Lib.registerTerminalResponseHandler (
    fun output -> Vdom_blit.process vdom_blit_app (TerminalOutput output)
  )
let () = print_endline ("hello world:"^(Location.pathname (Window.location window))); Window.set_onload window run