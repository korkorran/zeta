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
type configuration = {options : (Typeformat.value) StringMap.t; args : Typeformat.value StringMap.t}
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

let getOptionsFromLocation (head:Command.head) (location:string) : (string * Command.CommandOption.t) list =
  StringMap.bindings head.options
  |> List.filter (fun (_, opt) -> opt.Command.CommandOption.location = location)

let evalOption 
  (id, option: string* Command.CommandOption.t) 
  (restEntry: string list) 
  (currentConfig: configuration) 
  : (configuration * string list * string list) option =
  match option.Command.CommandOption.encoding with
  | Command.CommandOption.SingleString encodings -> (
    match restEntry with
    | [] -> None
    | he::te ->
        if List.exists (fun enc -> enc = he) encodings then
          let updatedOptions = StringMap.add id (Typeformat.VUnit) currentConfig.options in
          Some ({options=updatedOptions;args=currentConfig.args}, [he], te)
        else
          None )
  | Command.CommandOption.ValueSuffix (prefixes, typeformat) -> (
    match restEntry with
    | h1::h2::te -> 
      if List.exists (fun prefix -> prefix = h1) prefixes then
        let value_opt = Typeformat.value_of_token typeformat h2 in
        match value_opt with
        | None -> None
        | Some value ->
        let updatedOptions = StringMap.add id value currentConfig.options in
        Some ({options=updatedOptions;args=currentConfig.args}, [h1; h2], te)
      else
        None
    | _ -> None)

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
  | (h::_, []) -> 
    let availableOptions = List.flatten (List.map (getOptionsFromLocation head) availableOptionsLocation) in
    let succeededOptions = List.filter_map (fun opt -> evalOption opt restEntry currentConfig) availableOptions in
    (match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = evaluatedCommand;
        restCommand = restCommand;
      } |> fun state -> Ongoing (newConfig, state)
    | [] -> Overflow h)
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
      | SingleArgumentLocation l ->
        (match l with
          _ ->
            let he = List.hd restEntry in
            let newArgs = StringMap.add l (Typeformat.VString he) currentConfig.args in
            let newConfig = {options=currentConfig.options; args=newArgs} in
            Ongoing (newConfig, {
              evaluatedEntry = evaluatedEntry @ [he];
              restEntry = List.tl restEntry;
              evaluatedCommand = evaluatedCommand @ [hc];
              restCommand = List.tl restCommand;
            }))
      | Command.Token.ArgumentLocation l ->
        let encoding = (StringMap.find l head.args).encoding in
        match encoding with
        | Command.Argument.SingleString -> failwith "argument encoding should not be SingleString (reserved to SingleArgumentLocation), this should have been caught in parsing"
        | MultipleStrings (_min, max) ->
          let currentArgumentLocationState = StringMap.find_opt l currentConfig.args in
          (match currentArgumentLocationState with
          | None -> 
              let he = List.hd restEntry in
              let newArgs = StringMap.add l (Typeformat.VList [Typeformat.VString he]) currentConfig.args in
              let newConfig = {options=currentConfig.options; args=newArgs} in
              Ongoing (newConfig, {
                evaluatedEntry = evaluatedEntry @ [he];
                restEntry = List.tl restEntry;
                evaluatedCommand = evaluatedCommand;
                restCommand = restCommand;
              })
          | Some (Typeformat.VList l_args) -> let n = List.length l_args in
              if n >= max then
                Overflow (Format.sprintf "Argument %s expects at most %d values, but got at least %d" l max (n+1))
              else
                let he = List.hd restEntry in
                let newArgs = StringMap.add l (Typeformat.VList (l_args @ [Typeformat.VString he])) currentConfig.args in
                let newConfig = {options=currentConfig.options; args=newArgs} in
                let newEvaluatedCommand = if n + 1 >= max then evaluatedCommand @ [hc] else evaluatedCommand in
                let newRestCommand = if n + 1 >= max then List.tl restCommand else restCommand in
                Ongoing (newConfig, {
                  evaluatedEntry = evaluatedEntry @ [he];
                  restEntry = List.tl restEntry;
                  evaluatedCommand = newEvaluatedCommand;
                  restCommand = newRestCommand;
                })
          | Some _ -> failwith "This should not happen, argument location should always be associated with a list in the configuration")
(*         let he = List.hd restEntry in
        let newArgs = StringMap.add l (Typeformat.VString he) currentConfig.args in
        let newConfig = {options=currentConfig.options; args=newArgs} in
        Ongoing (newConfig, {
          evaluatedEntry = evaluatedEntry @ [he];
          restEntry = List.tl restEntry;
          evaluatedCommand = evaluatedCommand @ [hc];
          restCommand = List.tl restCommand;
        }) *)


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
  let tokens_opt = Entry_tokenizer.Main.parse_with_error (Lexing.from_string rawEntry) in
  match tokens_opt with
  | Some entry ->
  (* Placeholder implementation *)
  List.map (fun (path, head) ->
    let encodedPath = Command.EncodedPath.from_string path in
    let res = evalPath entry encodedPath head in
    (path, res)
  ) (Command.StringMap.bindings rawCommand)
  | None -> []