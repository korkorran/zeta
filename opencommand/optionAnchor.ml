
type optionConstraint = 
  | ExactLocation

type anchor = optionConstraint list

type anchorsMap = anchor StringMap.t


let getApplicableAnchors 
    (anchors:anchorsMap)
    (evaluatedCommand: FeaturePath.Token.t list)
    (restCommand: FeaturePath.Token.t list)
     : string list 
  =
  if evaluatedCommand = [] then
    []
  else
  StringMap.bindings anchors 
  |> List.filter (fun (id, constraints) ->
    List.for_all (fun constraint_ ->
      match constraint_ with
      | ExactLocation -> 
        (* Check if the anchor's id is in the first token of restCommand *)
        match restCommand with
        | [] -> false
        | firstToken::_ ->
          (match firstToken with
          | FeaturePath.Token.SingleOptionAnchor anchorId | FeaturePath.Token.Anchor anchorId -> anchorId = id
          | _ -> false)
    ) constraints
  )
  |> List.map (fun (id,_) -> id)