open Ocf
open Repr
open State


type entry = string list


(* (min, max) number of entry tokens accepted by an argument, max = None means unbounded *)
let argumentBounds : type a. a Feature.Argument.encoding -> int * int option = function
  | Feature.Argument.Single _ -> (1, Some 1)
  | Feature.Argument.Optional _ -> (0, Some 1)
  | Feature.Argument.Over (min, _) -> (min, None)
  | Feature.Argument.Between (min, max, _) -> (min, Some max)

(* evaluates the next entry token against a list argument location [hc] (named [l]),
   holding between [min] and [max] values (max = None means unbounded) *)
let evalListArgument (type a)
  (ongoingState : ongoingState)
  (hc : FeaturePath.Token.t)
  (l : string)
  (skipArgument : ongoingState)
  (min : int)
  (max : int option)
  (encoding : a TokenEncoding.tokenEncoding)
  : ongoingState list =
  let currentConfig = ongoingState.configuration in
  let currentStacks = ongoingState.evaluationStacks in
  let reachesMax n = match max with Some m -> n >= m | None -> false in
  let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
  let listTypeformat = Typeformat.TList typeformat in
  let l_args : a list = match StringMap.find_opt l currentConfig.args with
    | None -> []
    | Some (Typeformat.Value (tf, l_args)) -> (match Typeformat.equal_type listTypeformat tf with
      | Some Type.Equal -> l_args
      | None -> failwith "This should not happen, argument location (multiple strings) should always be associated with a list of its encoding in the configuration")
  in
  let n = List.length l_args in
  if reachesMax n then
    failwith (Format.asprintf "Too many arguments for %s, maximum is %d" l (Stdlib.Option.get max))
  else
  (* the argument can be left empty only before any value has been consumed *)
  let skipped =
    if min = 0 && not (StringMap.mem l currentConfig.args) then
      [{skipArgument with configuration = {Configuration.options=currentConfig.options; args=StringMap.add l (Typeformat.Value (listTypeformat, [])) currentConfig.args}}]
    else []
  in
  let he = List.hd currentStacks.restEntry in
  match Typeformat.value_of_token typeformat he with
  | None -> if skipped = [] then [{ongoingState with case = Unmatched hc}] else skipped
  | Some value ->
    let newConfig = {Configuration.options=currentConfig.options; args=StringMap.add l (Typeformat.Value (listTypeformat, l_args @ [value])) currentConfig.args} in
    let consumed evaluatedCommand restCommand = {ongoingState with configuration = newConfig; evaluationStacks = {
      evaluatedEntry = currentStacks.evaluatedEntry @ [he];
      restEntry = List.tl currentStacks.restEntry;
      evaluatedCommand;
      restCommand;
    }} in
    (* close the list: only once it holds enough values *)
    let closed = if n + 1 >= min then [consumed (currentStacks.evaluatedCommand @ [hc]) (List.tl currentStacks.restCommand)] else [] in
    (* keep the list open for more values: only while below the maximum *)
    let kept = if reachesMax (n + 1) then [] else [consumed currentStacks.evaluatedCommand currentStacks.restCommand] in
    skipped @ closed @ kept

let evalTokens 
  (head :Feature.feature) 
  (ongoingState : ongoingState)
  (availableOptionsAnchors: string list)
  : ongoingState list =
  let currentConfig = ongoingState.configuration in
  let currentStacks = ongoingState.evaluationStacks in
  match (currentStacks.restEntry, currentStacks.restCommand) with
  | [] , [] -> [{ongoingState with case = Success}]
  | ([], hc::_) -> (match hc with 
    FeaturePath.Token.Anchor _ -> 
      let tc = List.tl currentStacks.restCommand in
          [{ongoingState with evaluationStacks = {
            evaluatedEntry = currentStacks.evaluatedEntry;
            restEntry = currentStacks.restEntry;
            evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | FeaturePath.Token.ArgumentLocation l ->
        (let (Feature.Argument.Any arg) = StringMap.find l head.args in
        let emptyValue () = match arg.encoding with
          | Feature.Argument.Single _ -> assert false (* a Single argument requires one value, see argumentBounds *)
          | Feature.Argument.Optional encoding -> Typeformat.Value (Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding encoding), None)
          | Feature.Argument.Over (_, encoding) -> Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), [])
          | Feature.Argument.Between (_, _, encoding) -> Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), [])
        in
          (* a list argument already holding values was kept open when its values were consumed:
             the alternative that closes it has been generated at that time *)
          if fst (argumentBounds arg.encoding) = 0 && not (StringMap.mem l currentConfig.args) then 
            let newConfig = {Configuration.options=currentConfig.options; args=StringMap.add l (emptyValue ()) currentConfig.args} in
            [{ongoingState with configuration = newConfig; evaluationStacks = {
              evaluatedEntry = currentStacks.evaluatedEntry;
              restEntry = currentStacks.restEntry;
              evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
              restCommand = List.tl currentStacks.restCommand;
            }}]
          else
             [{ongoingState with case = Incomplete hc}])
    | _ -> [{ongoingState with case = Incomplete hc}])
  | (h::_, []) -> 
    let availableOptions = List.flatten (List.map (Option.getOptionsFromAnchor head) availableOptionsAnchors) in
    let succeededOptions = List.filter_map (fun opt -> Option.evalOption opt currentStacks.restEntry currentConfig) availableOptions in
    (match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = currentStacks.evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = currentStacks.evaluatedCommand;
        restCommand = currentStacks.restCommand;
      } |> fun state -> [{ongoingState with configuration = newConfig; evaluationStacks = state}]
    | [] -> [{ongoingState with case = Overflow h}])
  | (_, _) -> 
    let availableOptions = List.flatten (List.map (Option.getOptionsFromAnchor head) availableOptionsAnchors) in
    let succeededOptions = List.filter_map (fun opt -> Option.evalOption opt currentStacks.restEntry currentConfig) availableOptions in
    match succeededOptions with
    | (newConfig, evaluatedTokens, newRestEntry)::_ -> {
        evaluatedEntry = currentStacks.evaluatedEntry @ evaluatedTokens;
        restEntry = newRestEntry;
        evaluatedCommand = currentStacks.evaluatedCommand;
        restCommand = currentStacks.restCommand;
      } |> fun state -> [{ongoingState with configuration = newConfig; evaluationStacks = state}]
    | [] ->
      let hc = List.hd currentStacks.restCommand in
      match hc with
      | FeaturePath.Token.Directive d -> 
          let he = List.hd currentStacks.restEntry in
          if he = d then
            [{ongoingState with evaluationStacks = {
              evaluatedEntry = currentStacks.evaluatedEntry @ [he];
              restEntry = List.tl currentStacks.restEntry;
              evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
              restCommand = List.tl currentStacks.restCommand;
            }}]
          else
            [{ongoingState with case = Unmatched hc}]
      | FeaturePath.Token.Anchor _ ->
         let hc = List.hd currentStacks.restCommand in
         let tc = List.tl currentStacks.restCommand in
          [{ongoingState with evaluationStacks = {
            evaluatedEntry = currentStacks.evaluatedEntry;
            restEntry = currentStacks.restEntry;
            evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
            restCommand = tc;
          }}]
      | FeaturePath.Token.ArgumentLocation l ->
        let (Feature.Argument.Any arg) = StringMap.find l head.args in
        let skipArgument = {ongoingState with evaluationStacks = {
          evaluatedEntry = currentStacks.evaluatedEntry;
          restEntry = currentStacks.restEntry;
          evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
          restCommand = List.tl currentStacks.restCommand;
        }} in
        match arg.encoding with
        | Feature.Argument.Single encoding ->
          let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
          let he = List.hd currentStacks.restEntry in
          (match Typeformat.value_of_token typeformat he with
          | None -> [{ongoingState with case = Unmatched hc}]
          | Some value ->
            let newConfig = {Configuration.options=currentConfig.options; args=StringMap.add l (Typeformat.Value (typeformat, value)) currentConfig.args} in
            [{ongoingState with configuration = newConfig; evaluationStacks = {
              evaluatedEntry = currentStacks.evaluatedEntry @ [he];
              restEntry = List.tl currentStacks.restEntry;
              evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
              restCommand = List.tl currentStacks.restCommand;
            }}])
        | Feature.Argument.Optional encoding ->
          let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
          let noValueConfig = {Configuration.options=currentConfig.options; args=StringMap.add l (Typeformat.Value (Typeformat.TOption typeformat, None)) currentConfig.args} in
          let skipped = {skipArgument with configuration = noValueConfig} in
          let he = List.hd currentStacks.restEntry in
          (match Typeformat.value_of_token typeformat he with
          | None -> [skipped]
          | Some value ->
            let newConfig = {Configuration.options=currentConfig.options; args=StringMap.add l (Typeformat.Value (Typeformat.TOption typeformat, Some value)) currentConfig.args} in
            [{ongoingState with configuration = newConfig; evaluationStacks = {
              evaluatedEntry = currentStacks.evaluatedEntry @ [he];
              restEntry = List.tl currentStacks.restEntry;
              evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
              restCommand = List.tl currentStacks.restCommand;
            }}; skipped])
        | Feature.Argument.Over (min, encoding) ->
          evalListArgument ongoingState hc l skipArgument min None encoding
        | Feature.Argument.Between (min, max, encoding) ->
          evalListArgument ongoingState hc l skipArgument min (Some max) encoding
(*         let he = List.hd restEntry in
        let newArgs = StringMap.add l (Typeformat.VString he) currentConfig.args in
        let newConfig = {Configuration.options=currentConfig.options; args=newArgs} in
        Ongoing (newConfig, {
          evaluatedEntry = evaluatedEntry @ [he];
          restEntry = List.tl restEntry;
          evaluatedCommand = evaluatedCommand @ [hc];
          restCommand = List.tl restCommand;
        }) *)


type result = Success of Configuration.configuration 
  | Unmatched of FeaturePath.Token.t
  | Incomplete of FeaturePath.Token.t 
  | Overflow of string 
[@@deriving show]


let evalPath (entry:entry) (path:FeaturePath.EncodedPath.t) (head:Feature.feature) : result =
  let rec aux 
    (currentState_list: ongoingState list) 
    : (ongoingState list) =
    currentState_list 
    |> List.map (fun currentState ->
      (* Printf.printf "Current result: %s\n" (show_ongoingState currentState); *)
    match currentState.case with
    | Ongoing ->
        let anchors = Anchor.getApplicableAnchors 
          head.anchors 
          currentState.evaluationStacks.evaluatedCommand 
          currentState.evaluationStacks.restCommand in
        let nextStates = evalTokens head currentState anchors in
        aux nextStates
    | _ -> [currentState]) 
    |> List.flatten
  in
  let initState = { case= Ongoing;
       configuration = {Configuration.options=StringMap.empty;args=StringMap.empty};
       evaluationStacks ={
        evaluatedEntry = [];
        restEntry = entry;
        evaluatedCommand = [];
        restCommand = path;
      }} in
  let results = aux ([initState]) 
  in
  (* List.iter (fun r -> Printf.printf "Final result: %s\n" (show_ongoingState r)) results; *)
  let getSuccessfullOrLongestElement (lst: ongoingState list) =
    let success = List.find_opt (fun (r:ongoingState) -> match r.case with Success -> true | _ -> false) lst in
    match success with
    | Some s -> s
    | None -> List.fold_left (fun acc (r) -> if compare_ongoingState r acc > 0 then r else acc) initState lst
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