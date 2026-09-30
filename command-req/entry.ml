open Opencommand


type entry = string list
type configuration = {options : (Typeformat.value) StringMap.t; args : Typeformat.value StringMap.t}
[@@deriving show]


type evaluationState = {
  evaluatedEntry: string list;
  restEntry: string list;
  evaluatedCommand: FeaturePath.Token.t list;
  restCommand: FeaturePath.Token.t list;
}
[@@deriving show]

type ongoingCase = 
  | Ongoing
  | Success 
  | Unmatched of FeaturePath.Token.t 
  | Incomplete of FeaturePath.Token.t
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
    | (Unmatched t1, Unmatched t2) -> FeaturePath.Token.compare t1 t2
    | (Unmatched _, _) -> -1
    | (_, Unmatched _) -> 1
    | (Incomplete t1, Incomplete t2) -> FeaturePath.Token.compare t1 t2
    | (Incomplete _, _) -> -1
    | (_, Incomplete _) -> 1
    | (Overflow s1, Overflow s2) -> compare s1 s2
    
  

let getOptionsFromAnchor (feature:Feature.feature) (availableAnchor:string) : (string * Feature.CommandOption.t) list =
  StringMap.bindings feature.options
  |> List.filter (fun (_, opt) -> opt.Feature.CommandOption.location = availableAnchor)

let evalOption 
  (id, option: string* Feature.CommandOption.t) 
  (restEntry: string list) 
  (currentConfig: configuration) 
  : (configuration * string list * string list) option =
  match option.Feature.CommandOption.encoding with
  | Feature.CommandOption.SingleString encodings -> (
    match restEntry with
    | [] -> None
    | he::te ->
        if List.exists (fun enc -> enc = he) encodings then
          let updatedOptions = StringMap.add id (Typeformat.VUnit) currentConfig.options in
          Some ({options=updatedOptions;args=currentConfig.args}, [he], te)
        else
          None )
  | Feature.CommandOption.ValueSuffix (prefixes, encoding) -> (
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

(* (min, max) number of entry tokens accepted by an argument, max = None means unbounded *)
let argumentBounds : type a. a Feature.Argument.encoding -> int * int option = function
  | Feature.Argument.Single _ -> (1, Some 1)
  | Feature.Argument.Optional _ -> (0, Some 1)
  | Feature.Argument.Over (min, _) -> (min, None)
  | Feature.Argument.Between (min, max, _) -> (min, Some max)

(*let evalArgument
  (id, option: string* Command.argument) 
  (restEntry: string list) 
  (currentConfig: configuration) 
  : (configuration * string list * string list) option =
  match restEntry with
  | [] -> None
  | he::te -> *)

(* evaluates the next entry token against a list argument location [hc] (named [l]),
   holding between [min] and [max] values (max = None means unbounded) *)
let evalListArgument
  (ongoingResult : ongoingResult)
  (hc : FeaturePath.Token.t)
  (l : string)
  (skipArgument : ongoingResult)
  (min : int)
  (max : int option)
  (encoding : 'a TokenEncoding.tokenEncoding)
  : ongoingResult list =
  let currentConfig = ongoingResult.configuration in
  let currentState = ongoingResult.evaluationState in
  let reachesMax n = match max with Some m -> n >= m | None -> false in
  let l_args = match StringMap.find_opt l currentConfig.args with
    | None -> []
    | Some (Typeformat.VList l_args) -> l_args
    | Some _ -> failwith "This should not happen, argument location (multiple strings) should always be associated with a list in the configuration"
  in
  let n = List.length l_args in
  if reachesMax n then
    failwith (Format.asprintf "Too many arguments for %s, maximum is %d" l (Option.get max))
  else
  (* the argument can be left empty only before any value has been consumed *)
  let skipped =
    if min = 0 && not (StringMap.mem l currentConfig.args) then
      [{skipArgument with configuration = {options=currentConfig.options; args=StringMap.add l (Typeformat.VList []) currentConfig.args}}]
    else []
  in
  let he = List.hd currentState.restEntry in
  match Typeformat.value_of_token (TokenEncoding.typeformat_of_tokenEncoding encoding) he with
  | None -> if skipped = [] then [{ongoingResult with case = Unmatched hc}] else skipped
  | Some value ->
    let newConfig = {options=currentConfig.options; args=StringMap.add l (Typeformat.VList (l_args @ [value])) currentConfig.args} in
    let consumed evaluatedCommand restCommand = {ongoingResult with configuration = newConfig; evaluationState = {
      evaluatedEntry = currentState.evaluatedEntry @ [he];
      restEntry = List.tl currentState.restEntry;
      evaluatedCommand;
      restCommand;
    }} in
    (* close the list: only once it holds enough values *)
    let closed = if n + 1 >= min then [consumed (currentState.evaluatedCommand @ [hc]) (List.tl currentState.restCommand)] else [] in
    (* keep the list open for more values: only while below the maximum *)
    let kept = if reachesMax (n + 1) then [] else [consumed currentState.evaluatedCommand currentState.restCommand] in
    skipped @ closed @ kept

let evalTokens 
  (head :Feature.feature) 
  (ongoingResult : ongoingResult)
  (availableOptionsAnchors: string list)
  : ongoingResult list =
  let currentConfig = ongoingResult.configuration in
  let currentState = ongoingResult.evaluationState in
  match (currentState.restEntry, currentState.restCommand) with
  | [] , [] -> [{ongoingResult with case = Success}]
  | ([], hc::_) -> (match hc with 
    FeaturePath.Token.Anchor _ -> 
      let tc = List.tl currentState.restCommand in
          [{ongoingResult with evaluationState = {
            evaluatedEntry = currentState.evaluatedEntry;
            restEntry = currentState.restEntry;
            evaluatedCommand = currentState.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | FeaturePath.Token.ArgumentLocation l ->
        (let (Feature.Argument.Any arg) = StringMap.find l head.args in
        let emptyValue = match arg.encoding with
          | Feature.Argument.Single _ -> failwith "argument encoding should not be Single (reserved to SingleArgumentLocation), this should have been caught in parsing"
          | Feature.Argument.Optional _ -> Typeformat.VOption None
          | Feature.Argument.Over _ | Feature.Argument.Between _ -> Typeformat.VList []
        in
          (* a list argument already holding values was kept open when its values were consumed:
             the alternative that closes it has been generated at that time *)
          if fst (argumentBounds arg.encoding) = 0 && not (StringMap.mem l currentConfig.args) then 
            let newConfig = {options=currentConfig.options; args=StringMap.add l emptyValue currentConfig.args} in
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
    let availableOptions = List.flatten (List.map (getOptionsFromAnchor head) availableOptionsAnchors) in
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
    let availableOptions = List.flatten (List.map (getOptionsFromAnchor head) availableOptionsAnchors) in
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
      | FeaturePath.Token.Directive d -> 
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
      | FeaturePath.Token.Anchor _ ->
         let hc = List.hd currentState.restCommand in
         let tc = List.tl currentState.restCommand in
          [{ongoingResult with evaluationState = {
            evaluatedEntry = currentState.evaluatedEntry;
            restEntry = currentState.restEntry;
            evaluatedCommand = currentState.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | SingleArgumentLocation l ->
        (let (Feature.Argument.Any arg) = StringMap.find l head.args in
        match arg.encoding with
        | Feature.Argument.Optional _ | Feature.Argument.Over _ | Feature.Argument.Between _ -> failwith "argument encoding should be Single (other encodings are reserved to ArgumentLocation), this should have been caught in parsing"
        | Feature.Argument.Single encoding ->
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
      | FeaturePath.Token.ArgumentLocation l ->
        let (Feature.Argument.Any arg) = StringMap.find l head.args in
        let skipArgument = {ongoingResult with evaluationState = {
          evaluatedEntry = currentState.evaluatedEntry;
          restEntry = currentState.restEntry;
          evaluatedCommand = currentState.evaluatedCommand @ [hc];
          restCommand = List.tl currentState.restCommand;
        }} in
        match arg.encoding with
        | Feature.Argument.Single _ -> failwith "argument encoding should not be Single (reserved to SingleArgumentLocation), this should have been caught in parsing"
        | Feature.Argument.Optional encoding ->
          let noValueConfig = {options=currentConfig.options; args=StringMap.add l (Typeformat.VOption None) currentConfig.args} in
          let skipped = {skipArgument with configuration = noValueConfig} in
          let he = List.hd currentState.restEntry in
          (match Typeformat.value_of_token (TokenEncoding.typeformat_of_tokenEncoding encoding) he with
          | None -> [skipped]
          | Some value ->
            let newConfig = {options=currentConfig.options; args=StringMap.add l (Typeformat.VOption (Some value)) currentConfig.args} in
            [{ongoingResult with configuration = newConfig; evaluationState = {
              evaluatedEntry = currentState.evaluatedEntry @ [he];
              restEntry = List.tl currentState.restEntry;
              evaluatedCommand = currentState.evaluatedCommand @ [hc];
              restCommand = List.tl currentState.restCommand;
            }}; skipped])
        | Feature.Argument.Over (min, encoding) ->
          evalListArgument ongoingResult hc l skipArgument min None encoding
        | Feature.Argument.Between (min, max, encoding) ->
          evalListArgument ongoingResult hc l skipArgument min (Some max) encoding
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
  | Unmatched of FeaturePath.Token.t
  | Incomplete of FeaturePath.Token.t 
  | Overflow of string 
[@@deriving show]


let evalPath (entry:entry) (path:FeaturePath.EncodedPath.t) (head:Feature.feature) : result =
  let rec aux 
    (currentResult_list: ongoingResult list) 
    : (ongoingResult list) =
    currentResult_list 
    |> List.map (fun currentResult ->
      (* Printf.printf "Current result: %s\n" (show_ongoingResult currentResult); *)
    match currentResult.case with
    | Ongoing ->
        let anchors = OptionAnchor.getApplicableAnchors 
          head.anchors 
          currentResult.evaluationState.evaluatedCommand 
          currentResult.evaluationState.restCommand in
        let nextResult = evalTokens head currentResult anchors in
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

let evalConfiguration (rawEntry:string) (rawCommand:Feature.featureMap) : (string * result) list =
  (* let command = Command.parseEncodedCommand rawCommand in *)
  let tokens_opt = Entry_tokenizer.Main.parse_with_error (Lexing.from_string rawEntry) in
  match tokens_opt with
  | Some entry ->
  (* Placeholder implementation *)
  List.map (fun (feature, head) ->
    let encodedPath = head.Feature.path in
    let res = evalPath entry encodedPath head in
    (feature, res)
  ) (StringMap.bindings rawCommand)
  | None -> []