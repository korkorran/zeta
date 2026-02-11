module StringMap = struct
  include Map.Make(String);;
  let pp pp_v fmt m =
    let bindings = bindings m in
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%s: %a" k pp_v v
    in
    List.iter (fun b ->
      pp_binding fmt b;
      Format.fprintf fmt "\n"
    ) bindings
end

type entry = string list
type configuration = {options : Yojson.Basic.t StringMap.t; args : Yojson.Basic.t StringMap.t}
[@@deriving show]

let tokenize str =
    String.split_on_char ' ' str

type evaluationState = {
  evaluatedEntry: string list;
  restEntry: string list;
  evaluatedCommand: Command.Token.t list;
  restCommand: Command.Token.t list;
}
type ongoingResult = 
    Success of configuration 
  | Unmatched of Command.Token.t
  | Incomplete of Command.Token.t 
  | Overflow of string 
  | Ongoing of configuration * evaluationState

let getOptionsFromLocation (head:Command.head) (location:string) : (string * Command.commandOption) list =
  StringMap.bindings head.options
  |> List.filter (fun (_, opt) -> opt.Command.location = location)

let evalOption 
  (id, option: string* Command.commandOption) 
  (restEntry: string list) 
  (currentConfig: configuration) 
  : (configuration * string list * string list) option =
  match restEntry with
  | [] -> None
  | he::te ->
      if List.exists (fun enc -> enc = he) option.Command.encodings then
        let updatedOptions = StringMap.add id (`String "unit") currentConfig.options in
        Some ({options=updatedOptions;args=currentConfig.args}, [he], te)
      else
        None

(*let evalArgument
  (id, option: string* Command.argument) 
  (restEntry: string list) 
  (currentConfig: configuration) 
  : (configuration * string list * string list) option =
  match restEntry with
  | [] -> None
  | he::te -> *)

let evalTokens 
  (head :Command.head)
  (evaluatedEntry, restEntry: string list * string list) 
  (evaluatedCommand, restCommand: Command.Token.t list * Command.Token.t list) 
  (availableOptionsLocation: string list)
  (currentConfig: configuration)
  : ongoingResult =
  match (restEntry, restCommand) with
  | [] , [] -> Success currentConfig
  | ([], hc::_) -> (match hc with 
    Command.Token.OptionLocation _ | SingleOptionLocation _ -> 
      let tc = List.tl restCommand in
          Ongoing (currentConfig, {
            evaluatedEntry = evaluatedEntry;
            restEntry = restEntry;
            evaluatedCommand = evaluatedCommand @ [hc];
            restCommand = tc;
          })
    | _ -> Incomplete hc)
  | (h::_, []) -> Overflow h
  | (_, _) -> 
    let availableOptions = List.flatten (List.map (getOptionsFromLocation head) availableOptionsLocation) in
    let succeededOptions = List.filter_map (fun opt -> evalOption opt restEntry currentConfig) availableOptions in
    match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = evaluatedCommand;
        restCommand = restCommand;
      } |> fun state -> Ongoing (newConfig, state)
    | [] ->
      let hc = List.hd restCommand in
      match hc with
      | Command.Token.Directive d -> 
          let he = List.hd restEntry in
          if he = d then
            Ongoing (currentConfig, {
              evaluatedEntry = evaluatedEntry @ [he];
              restEntry = List.tl restEntry;
              evaluatedCommand = evaluatedCommand @ [hc];
              restCommand = List.tl restCommand;
            })
          else
            Unmatched hc
      | Command.Token.OptionLocation _ | SingleOptionLocation _  ->
         let hc = List.hd restCommand in
         let tc = List.tl restCommand in
          Ongoing (currentConfig, {
            evaluatedEntry = evaluatedEntry;
            restEntry = restEntry;
            evaluatedCommand = evaluatedCommand @ [hc];
            restCommand = tc;
          })
      | Command.Token.ArgumentLocation l | SingleArgumentLocation l ->
        match l with
          _ ->
            let he = List.hd restEntry in
            let newArgs = StringMap.add l (`String he) currentConfig.args in
            let newConfig = {options=currentConfig.options; args=newArgs} in
            Ongoing (newConfig, {
              evaluatedEntry = evaluatedEntry @ [he];
              restEntry = List.tl restEntry;
              evaluatedCommand = evaluatedCommand @ [hc];
              restCommand = List.tl restCommand;
            })


type result = Success of configuration 
  | Unmatched of Command.Token.t
  | Incomplete of Command.Token.t 
  | Overflow of string 
[@@deriving show]

let getAllOptionsLocations (head:Command.head) : string list =
  StringMap.bindings head.optionsLocations |> List.map (fun (id,_) -> id)
let evalPath (entry:entry) (path:Command.EncodedPath.t) (head:Command.head) : result =
  let rec aux 
    (currentResult: ongoingResult) 
    : (ongoingResult) =
    match currentResult with
    | Ongoing (conf, state) ->
        let optionsLocations = getAllOptionsLocations head in
        let nextResult = evalTokens head (state.evaluatedEntry, state.restEntry) (state.evaluatedCommand, state.restCommand) optionsLocations conf in
        aux nextResult
    | _ -> (currentResult)
  in
  let result = aux (
    Ongoing 
      ({options=StringMap.empty;args=StringMap.empty}, 
      {
        evaluatedEntry = [];
        restEntry = entry;
        evaluatedCommand = [];
        restCommand = path;
      })) 
  in
  match result with
  | Success conf -> Success conf
  | Unmatched x -> Unmatched x
  | Incomplete x -> Incomplete x
  | Overflow x -> Overflow x
  | Ongoing _ -> failwith "This should not happen, Ongoing should be fully evaluated in aux"

let evalConfiguration (rawEntry:string) (rawCommand:Command.rawCommand) : (string * result) list =
  (* let command = Command.parseEncodedCommand rawCommand in *)
  let entry = tokenize rawEntry in
  (* Placeholder implementation *)
  List.map (fun (path, head) ->
    let encodedPath = Command.EncodedPath.from_string path in
    let res = evalPath entry encodedPath head in
    (path, res)
  ) (Command.StringMap.bindings rawCommand)