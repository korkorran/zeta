open Ocf
open Repr
open State


type entry = string list


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
        Argument.evalArgument ongoingState hc l (StringMap.find l head.args)
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
        Argument.evalArgument ongoingState hc l (StringMap.find l head.args)



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