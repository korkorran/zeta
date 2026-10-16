open Ocf
open Repr
open State

let evalTokens
  (head : Feature.feature)
  ({tokenStacks = stacks; _} as ongoingState : ongoingState)
  (applicableOptionsAnchors : string list)
  : ongoingState list =
  (* the first option of the applicable anchors matching the head of the entry *)
  let evalOptions () =
    applicableOptionsAnchors
    |> List.concat_map (Option.getOptionsFromAnchor head)
    |> List.find_map (fun opt -> Option.evalOption opt ongoingState)
  in
  (* the command token [hc] is consumed without consuming any entry token *)
  let skip hc restCommand =
    [{ongoingState with tokenStacks = {stacks with evaluatedCommand = stacks.evaluatedCommand @ [hc]; restCommand}}]
  in
  match stacks.restEntry, stacks.restCommand with
  | [], [] -> [{ongoingState with case = Success}]
  | [], (FeaturePath.Token.Anchor _ as hc) :: tc -> skip hc tc
  | [], FeaturePath.Token.ArgumentLocation _ :: _ -> Argument.evalArgument head.args ongoingState
  | [], hc :: _ -> [{ongoingState with case = Incomplete hc}]
  | he :: _, [] ->
    begin match evalOptions () with
    | Some state -> [state]
    | None -> [{ongoingState with case = Overflow he}]
    end
  | he :: te, hc :: tc ->
    match evalOptions (), hc with
    | Some state, _ -> [state]
    | None, FeaturePath.Token.Directive d when he = d ->
      [{ongoingState with tokenStacks = {
        evaluatedEntry = stacks.evaluatedEntry @ [he];
        restEntry = te;
        evaluatedCommand = stacks.evaluatedCommand @ [hc];
        restCommand = tc;
      }}]
    | None, FeaturePath.Token.Directive _ -> [{ongoingState with case = Unmatched hc}]
    | None, FeaturePath.Token.Anchor _ -> skip hc tc
    | None, FeaturePath.Token.ArgumentLocation _ -> Argument.evalArgument head.args ongoingState


type result = Success of Configuration.configuration 
  | Unmatched of FeaturePath.Token.t
  | Incomplete of FeaturePath.Token.t 
  | Overflow of string 
[@@deriving show]


let evalFeature (entry:string list) (head:Feature.feature) : result =
  let path = head.path in
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
          currentState.tokenStacks.evaluatedCommand 
          currentState.tokenStacks.restCommand in
        let nextStates = evalTokens head currentState anchors in
        aux nextStates
    | _ -> [currentState]) 
    |> List.flatten
  in
  let initState = { case= Ongoing;
       configuration = {Configuration.options=StringMap.empty;args=StringMap.empty};
       tokenStacks ={
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