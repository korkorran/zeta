

type entry = string list
type configuration = {options : (Typeformat.value) StringMap.t; args : Typeformat.value StringMap.t}
[@@deriving show]


type evaluationState = {
  evaluatedEntry: string list;
  restEntry: string list;
  evaluatedCommand: Command.Token.t list;
  restCommand: Command.Token.t list;
}
[@@deriving show]

type ongoingCase = 
  | Ongoing
  | Success 
  | Unmatched of Command.Token.t 
  | Incomplete of Command.Token.t
  | Overflow of string
[@@deriving show]

type ongoingResult = {
  configuration : configuration;
  evaluationState : evaluationState;
  case : ongoingCase;
}
[@@deriving show]

let compare_ongoingResult r1 r2 =
  let entry_cmp = compare (List.length r1.evaluationState.evaluatedEntry) (List.length r2.evaluationState.evaluatedEntry) in
  if entry_cmp <> 0 then entry_cmp
  else
    match (r1.case, r2.case) with
    | (Success, Success) -> 0
    | (Success, _) -> -1
    | (_, Success) -> 1
    | (Ongoing, Ongoing) -> 0
    | (Ongoing, _) -> -1
    | (_, Ongoing) -> 1
    | (Unmatched t1, Unmatched t2) -> Command.Token.compare t1 t2
    | (Unmatched _, _) -> -1
    | (_, Unmatched _) -> 1
    | (Incomplete t1, Incomplete t2) -> Command.Token.compare t1 t2
    | (Incomplete _, _) -> -1
    | (_, Incomplete _) -> 1
    | (Overflow s1, Overflow s2) -> compare s1 s2
    
  

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
  | Command.CommandOption.ValueSuffix (prefixes, encoding) -> (
    match restEntry with
    | h1::h2::te -> 
      if List.exists (fun prefix -> prefix = h1) prefixes then
        let value_opt = Typeformat.value_of_token (TokenEncoding.typeformat_of_tokenEncoding encoding) h2 in
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
  (ongoingResult : ongoingResult)
  (availableOptionsLocation: string list)
  : ongoingResult list =
  let currentConfig = ongoingResult.configuration in
  let currentState = ongoingResult.evaluationState in
  match (currentState.restEntry, currentState.restCommand) with
  | [] , [] -> [{ongoingResult with case = Success}]
  | ([], hc::_) -> (match hc with 
    Command.Token.OptionLocation _ | SingleOptionLocation _ -> 
      let tc = List.tl currentState.restCommand in
          [{ongoingResult with evaluationState = {
            evaluatedEntry = currentState.evaluatedEntry;
            restEntry = currentState.restEntry;
            evaluatedCommand = currentState.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | Command.Token.ArgumentLocation l ->
        (let encoding = (StringMap.find l head.args).encoding in
        match encoding with
        | Command.Argument.Single _ -> failwith "argument encoding should not be SingleString (reserved to SingleArgumentLocation), this should have been caught in parsing"
        | ArgList (_tf, min, _max) ->
          if min = 0 then 
            let newConfig = {options=currentConfig.options; args=StringMap.add l (Typeformat.VList []) currentConfig.args} in
            [{ongoingResult with configuration = newConfig; evaluationState = {
              evaluatedEntry = currentState.evaluatedEntry;
              restEntry = currentState.restEntry;
              evaluatedCommand = currentState.evaluatedCommand @ [hc];
              restCommand = List.tl currentState.restCommand;
            }}]
          else
             [{ongoingResult with case = Incomplete hc}])
    | _ -> [{ongoingResult with case = Incomplete hc}])
  | (h::_, []) -> 
    let availableOptions = List.flatten (List.map (getOptionsFromLocation head) availableOptionsLocation) in
    let succeededOptions = List.filter_map (fun opt -> evalOption opt currentState.restEntry currentConfig) availableOptions in
    (match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = currentState.evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = currentState.evaluatedCommand;
        restCommand = currentState.restCommand;
      } |> fun state -> [{ongoingResult with configuration = newConfig; evaluationState = state}]
    | [] -> [{ongoingResult with case = Overflow h}])
  | (_, _) -> 
    let availableOptions = List.flatten (List.map (getOptionsFromLocation head) availableOptionsLocation) in
    let succeededOptions = List.filter_map (fun opt -> evalOption opt currentState.restEntry currentConfig) availableOptions in
    match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = currentState.evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = currentState.evaluatedCommand;
        restCommand = currentState.restCommand;
      } |> fun state -> [{ongoingResult with configuration = newConfig; evaluationState = state}]
    | [] ->
      let hc = List.hd currentState.restCommand in
      match hc with
      | Command.Token.Directive d -> 
          let he = List.hd currentState.restEntry in
          if he = d then
            [{ongoingResult with evaluationState = {
              evaluatedEntry = currentState.evaluatedEntry @ [he];
              restEntry = List.tl currentState.restEntry;
              evaluatedCommand = currentState.evaluatedCommand @ [hc];
              restCommand = List.tl currentState.restCommand;
            }}]
          else
            [{ongoingResult with case = Unmatched hc}]
      | Command.Token.OptionLocation _ | SingleOptionLocation _  ->
         let hc = List.hd currentState.restCommand in
         let tc = List.tl currentState.restCommand in
          [{ongoingResult with evaluationState = {
            evaluatedEntry = currentState.evaluatedEntry;
            restEntry = currentState.restEntry;
            evaluatedCommand = currentState.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | SingleArgumentLocation l ->
        (let encoding = (StringMap.find l head.args).encoding in
        match encoding with
        | Command.Argument.ArgList _ -> failwith "argument encoding should not be ArgList (reserved to ArgumentLocation), this should have been caught in parsing"
        | Command.Argument.Single encoding ->
            let he = List.hd currentState.restEntry in
            let value_opt = Typeformat.value_of_token (TokenEncoding.typeformat_of_tokenEncoding encoding) he in
            match value_opt with
              | None -> [{ongoingResult with case = Unmatched hc}]
              | Some value ->
            let newArgs = StringMap.add l value currentConfig.args in
            let newConfig = {options=currentConfig.options; args=newArgs} in
            [{ongoingResult with configuration = newConfig; evaluationState = {
              evaluatedEntry = currentState.evaluatedEntry @ [he];
              restEntry = List.tl currentState.restEntry;
              evaluatedCommand = currentState.evaluatedCommand @ [hc];
              restCommand = List.tl currentState.restCommand;
            }}])
      | Command.Token.ArgumentLocation l ->
        let encoding = (StringMap.find l head.args).encoding in
        match encoding with
        | Command.Argument.Single _ -> failwith "argument encoding should not be SingleString (reserved to SingleArgumentLocation), this should have been caught in parsing"
        | ArgList (_tf, min, max) ->
          let currentArgumentLocationState = StringMap.find_opt l currentConfig.args in
          (match currentArgumentLocationState with
          | None -> 
              let he = List.hd currentState.restEntry in
              let newArgs = StringMap.add l (Typeformat.VList [Typeformat.VString he]) currentConfig.args in
              let newConfig = {options=currentConfig.options; args=newArgs} in
              let result = [{ongoingResult with configuration = newConfig; evaluationState = {
                evaluatedEntry = currentState.evaluatedEntry @ [he];
                restEntry = List.tl currentState.restEntry;
                evaluatedCommand = currentState.evaluatedCommand;
                restCommand = currentState.restCommand;
              }};
              {ongoingResult with configuration = newConfig; evaluationState = {
                evaluatedEntry = currentState.evaluatedEntry @ [he];
                restEntry = List.tl currentState.restEntry;
                evaluatedCommand = currentState.evaluatedCommand @ [hc];
                restCommand = List.tl currentState.restCommand;
              }};
              ] in 
              if min = 0 then 
                {ongoingResult with configuration = newConfig; evaluationState = {
                evaluatedEntry = currentState.evaluatedEntry;
                restEntry = currentState.restEntry;
                evaluatedCommand = currentState.evaluatedCommand @ [hc];
                restCommand = List.tl currentState.restCommand;
              }}::result
              else result
          | Some (Typeformat.VList l_args) -> let n = List.length l_args in
              if n >= max then
                failwith (Format.asprintf "Too many arguments for %s, maximum is %d" l max)
              else
                let he = List.hd currentState.restEntry in
                let newArgs = StringMap.add l (Typeformat.VList (l_args @ [Typeformat.VString he])) currentConfig.args in
                let newConfig = {options=currentConfig.options; args=newArgs} in
                let newEvaluatedCommand = currentState.evaluatedCommand @ [hc] in
                let newRestCommand =  List.tl currentState.restCommand in
                if n + 1 >= max then
                  [{ongoingResult with configuration = newConfig; evaluationState = {
                    evaluatedEntry = currentState.evaluatedEntry @ [he];
                    restEntry = List.tl currentState.restEntry;
                    evaluatedCommand = newEvaluatedCommand;
                    restCommand = newRestCommand;
                  }}]
                else [
                  {ongoingResult with configuration = newConfig; evaluationState = {
                    evaluatedEntry = currentState.evaluatedEntry @ [he];
                    restEntry = List.tl currentState.restEntry;
                    evaluatedCommand = newEvaluatedCommand;
                    restCommand = newRestCommand;
                  }};
                  {ongoingResult with configuration = newConfig; evaluationState = {
                    evaluatedEntry = currentState.evaluatedEntry @ [he];
                    restEntry = List.tl currentState.restEntry;
                    evaluatedCommand = currentState.evaluatedCommand;
                    restCommand = currentState.restCommand;
                  }}
                ]
          | Some _ -> failwith "This should not happen, argument location (multiple strings) should always be associated with a list in the configuration")
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
    (currentResult_list: ongoingResult list) 
    : (ongoingResult list) =
    currentResult_list 
    |> List.map (fun currentResult ->
      (* Printf.printf "Current result: %s\n" (show_ongoingResult currentResult); *)
    match currentResult.case with
    | Ongoing ->
        let optionsLocations = getAllOptionsLocations head in
        let nextResult = evalTokens head currentResult optionsLocations in
        aux nextResult
    | _ -> [currentResult]) 
    |> List.flatten
  in
  let initState = { case= Ongoing;
       configuration = {options=StringMap.empty;args=StringMap.empty};
       evaluationState ={
        evaluatedEntry = [];
        restEntry = entry;
        evaluatedCommand = [];
        restCommand = path;
      }} in
  let results = aux ([initState]) 
  in
  (* List.iter (fun r -> Printf.printf "Final result: %s\n" (show_ongoingResult r)) results; *)
  let getSuccessfullOrLongestElement (lst: ongoingResult list) =
    let success = List.find_opt (fun (r:ongoingResult) -> match r.case with Success -> true | _ -> false) lst in
    match success with
    | Some s -> s
    | None -> List.fold_left (fun acc (r) -> if compare_ongoingResult r acc > 0 then r else acc) initState lst
  in
  let finalResult = getSuccessfullOrLongestElement results in
  match finalResult.case with
  | Success -> Success finalResult.configuration
  | Unmatched x -> Unmatched x
  | Incomplete x -> Incomplete x
  | Overflow x -> Overflow x
  | Ongoing -> Overflow "Evaluation did not finish, this should not happen"

let evalConfiguration (rawEntry:string) (rawCommand:Command.commandFeatures) : (string * result) list =
  (* let command = Command.parseEncodedCommand rawCommand in *)
  let tokens_opt = Entry_tokenizer.Main.parse_with_error (Lexing.from_string rawEntry) in
  match tokens_opt with
  | Some entry ->
  (* Placeholder implementation *)
  List.map (fun (feature, head) ->
    let encodedPath = Command.EncodedPath.from_string head.Command.path in
    let res = evalPath entry encodedPath head in
    (feature, res)
  ) (StringMap.bindings rawCommand)
  | None -> []