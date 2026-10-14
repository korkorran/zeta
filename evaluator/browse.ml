open Ocf
open Repr

let registry = Registry.Index.registry

(* descriptions of the format *)

let path_to_string path =
  String.concat " " (List.map FeaturePath.Token.token_to_string path)

let stringFormat_to_string = function
  | None -> "string"
  | Some TokenEncoding.QuotedString -> "quoted string"
  | Some TokenEncoding.SingleWord -> "single word"
  | Some TokenEncoding.FilePath -> "file path"

let rec tokenEncoding_to_string : type a. a TokenEncoding.tokenEncoding -> string = function
  | TokenEncoding.FixedString strings -> String.concat " | " strings
  | TokenEncoding.FormattedString format_opt -> "<" ^ stringFormat_to_string format_opt ^ ">"
  | TokenEncoding.Integer -> "<int>"
  | TokenEncoding.Enum options -> "{" ^ String.concat " | " options ^ "}"
  | TokenEncoding.Prefixed (prefixes, encoding) ->
    String.concat " | " (List.map (fun prefix -> prefix ^ tokenEncoding_to_string encoding) prefixes)

let written_to_string : type a. a Feature.CommandOption.written -> string = function
  | Feature.CommandOption.SingleToken encoding -> tokenEncoding_to_string encoding
  | Feature.CommandOption.ValueNextToken (keyEncoding, encoding) ->
    tokenEncoding_to_string keyEncoding ^ " " ^ tokenEncoding_to_string encoding

let optionEncoding_to_string : type a. a Feature.CommandOption.encoding -> string = function
  | Feature.CommandOption.Once written -> written_to_string written ^ "  (once)"
  | Feature.CommandOption.Repeatable written -> written_to_string written ^ "  (repeatable)"

let argumentEncoding_to_string : type a. a Feature.Argument.encoding -> string = function
  | Feature.Argument.Single encoding -> tokenEncoding_to_string encoding
  | Feature.Argument.Optional encoding -> "optional " ^ tokenEncoding_to_string encoding
  | Feature.Argument.Over (min, encoding) -> Printf.sprintf "at least %d × %s" min (tokenEncoding_to_string encoding)
  | Feature.Argument.Between (min, max, encoding) -> Printf.sprintf "%d to %d × %s" min max (tokenEncoding_to_string encoding)

(* the OCaml type of a configuration value *)
let rec typeformat_to_string : type a. a Typeformat.typeformat -> string = function
  | Typeformat.TUnit -> "unit"
  | Typeformat.TInt -> "int"
  | Typeformat.TFloat -> "float"
  | Typeformat.TBool -> "bool"
  | Typeformat.TString _ -> "string"
  | Typeformat.TEnum _ -> "string"
  | Typeformat.TList tf -> typeformat_to_string tf ^ " list"
  | Typeformat.TOption tf -> typeformat_to_string tf ^ " option"
  | Typeformat.TPair (a, b) -> "(" ^ typeformat_to_string a ^ " * " ^ typeformat_to_string b ^ ")"

let type_of templates id =
  match StringMap.find_opt id templates with
  | Some (Typeformat.Any tf) -> typeformat_to_string tf
  | None -> "?"

let constraint_to_string = function
  | Feature.OptionAnchor.ExactLocation -> "exact location"
  | Feature.OptionAnchor.AfterLastDirective -> "after last directive"

(* the lines describing a feature *)
let feature_lines (id : string) (feature : Feature.feature) : string list =
  let template = Configuration.getConfigurationTemplate feature in
  let section title = function
    | [] -> [""; title; "  (none)"]
    | lines -> [""; title] @ lines
  in
  let args = StringMap.bindings feature.args |> List.concat_map (fun (name, Feature.Argument.Any arg) ->
    [ Printf.sprintf "  <%s>  %s  : %s" name (argumentEncoding_to_string arg.encoding) (type_of template.argsT name);
      "      " ^ arg.description ])
  in
  let anchors = StringMap.bindings feature.anchors |> List.map (fun (name, constraints) ->
    let constraints = match constraints with
      | [] -> "anywhere"
      | constraints -> String.concat ", " (List.map constraint_to_string constraints)
    in
    Printf.sprintf "  §%s  : %s" name constraints)
  in
  let options = StringMap.bindings feature.options |> List.concat_map (fun (name, Feature.CommandOption.Any opt) ->
    [ Printf.sprintf "  %s  [§%s]  %s  : %s" name opt.location (optionEncoding_to_string opt.encoding) (type_of template.optionsT name);
      "      " ^ opt.description ])
  in
  [ id; feature.description; ""; "Path:  " ^ path_to_string feature.path ]
  @ section "Arguments:" args
  @ section "Anchors:" anchors
  @ section "Options:" options

(* non interactive listing, when the output is not a terminal *)

let print_registry () =
  StringMap.iter (fun _ (command : Command.command) ->
    Printf.printf "%s (%s) : %s\n" command.info.commandName command.info.version command.info.description;
    StringMap.iter (fun id (feature : Feature.feature) ->
      Printf.printf "  %s : %s\n" id (path_to_string feature.path)
    ) command.features;
    print_newline ()
  ) registry

(* interactive browser *)

type screen =
  | Commands of { selected : int }
  | Features of { command : Command.command; selected : int }
  | Detail of { command : Command.command; selected : int; scroll : int }

let commands = List.map snd (StringMap.bindings registry)

let features_of (command : Command.command) = StringMap.bindings command.features

let title_style = { LTerm_style.none with bold = Some true }
let selected_style = { LTerm_style.none with reverse = Some true }
let help_style = { LTerm_style.none with foreground = Some LTerm_style.blue }

(* title, lines of the screen, index of the selected line (if the screen is a list) and first visible line *)
let contents = function
  | Commands { selected } ->
    let lines = List.map (fun (command : Command.command) ->
      Printf.sprintf "%-8s %s" command.info.commandName command.info.description) commands in
    ("Commands", lines, Some selected)
  | Features { command; selected } ->
    let lines = List.map (fun (id, (feature : Feature.feature)) ->
      Printf.sprintf "%-24s %s" id (path_to_string feature.path)) (features_of command) in
    (command.info.commandName ^ " : features", lines, Some selected)
  | Detail { command; selected; _ } ->
    let (id, feature) = List.nth (features_of command) selected in
    (command.info.commandName ^ " : " ^ id, feature_lines id feature, None)

let help = function
  | Commands _ -> "↑/↓ move   Enter select   q quit"
  | Features _ -> "↑/↓ move   Enter select   ← back   q quit"
  | Detail _ -> "↑/↓ scroll   ← back   q quit"

let draw screen ui matrix =
  let ctx = LTerm_draw.context matrix (LTerm_ui.size ui) in
  LTerm_draw.clear ctx;
  let { LTerm_geom.rows; _ } = LTerm_draw.size ctx in
  let (title, lines, selected) = contents screen in
  LTerm_draw.draw_string ctx 0 1 ~style:title_style (Zed_string.of_utf8 title);
  (* lines are drawn between the title and the help line *)
  let height = max 1 (rows - 3) in
  let first = match screen, selected with
    | Detail { scroll; _ }, _ -> scroll
    | _, Some s -> max 0 (s - height + 1)
    | _, None -> 0
  in
  List.iteri (fun i line ->
    if i >= first && i < first + height then
      let style = if Some i = selected then selected_style else LTerm_style.none in
      LTerm_draw.draw_string ctx (i - first + 2) 2 ~style (Zed_string.of_utf8 line)
  ) lines;
  LTerm_draw.draw_string ctx (rows - 1) 1 ~style:help_style (Zed_string.of_utf8 (help screen))

let length screen =
  let (_, lines, _) = contents screen in
  List.length lines

let move screen delta =
  let clamp n i = max 0 (min (n - 1) i) in
  match screen with
  | Commands { selected } -> Commands { selected = clamp (length screen) (selected + delta) }
  | Features { command; selected } -> Features { command; selected = clamp (length screen) (selected + delta) }
  | Detail { command; selected; scroll } -> Detail { command; selected; scroll = clamp (length screen) (scroll + delta) }

let enter = function
  | Commands { selected } -> Features { command = List.nth commands selected; selected = 0 }
  | Features { command; selected } -> Detail { command; selected; scroll = 0 }
  | Detail _ as screen -> screen

(* None when leaving the browser *)
let back = function
  | Commands _ -> None
  | Features { command; _ } ->
    let rec index i = function
      | [] -> 0
      | (c : Command.command) :: rest -> if c.info.commandName = command.info.commandName then i else index (i + 1) rest
    in
    Some (Commands { selected = index 0 commands })
  | Detail { command; selected; _ } -> Some (Features { command; selected })

let rec loop ui screen =
  let open Lwt.Syntax in
  let* event = LTerm_ui.wait ui in
  let next = match event with
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Up; _ } -> Some (move !screen (-1))
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Down; _ } -> Some (move !screen 1)
    | LTerm_event.Key { LTerm_key.code = (LTerm_key.Enter | LTerm_key.Right); _ } -> Some (enter !screen)
    | LTerm_event.Key { LTerm_key.code = (LTerm_key.Left | LTerm_key.Backspace | LTerm_key.Escape); _ } -> back !screen
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char c; _ } when Uchar.to_int c = Char.code 'q' -> None
    | _ -> Some !screen
  in
  match next with
  | None -> Lwt.return_unit
  | Some next ->
    screen := next;
    LTerm_ui.draw ui;
    loop ui screen

let browse () =
  let open Lwt.Syntax in
  let* term = Lazy.force LTerm.stdout in
  let screen = ref (Commands { selected = 0 }) in
  let* ui = LTerm_ui.create term (fun ui matrix -> draw !screen ui matrix) in
  Lwt.finalize (fun () -> loop ui screen) (fun () -> LTerm_ui.quit ui)

let run () =
  if Unix.isatty Unix.stdout then Lwt_main.run (browse ()) else print_registry ()
