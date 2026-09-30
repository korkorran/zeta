
let checkConstraint (evaluatedCommand) (restCommand) (constraint_) (id) =
  match constraint_ with
  | Feature.OptionAnchor.ExactLocation -> 
    (* Check if the anchor's id is in the first token of restCommand *)
    (match restCommand with
    | [] -> false
    | firstToken::_ ->
      (match firstToken with
      | FeaturePath.Token.Anchor anchorId -> anchorId = id
      | _ -> false))
  | Feature.OptionAnchor.AfterLastDirective -> 
    (* if the anchor's id is in the evaluatedCommand, then the constraint is satisfied *)
    if List.exists (fun token -> match token with FeaturePath.Token.Anchor a -> a = id | _ -> false) evaluatedCommand then
      true
    else
      (* Check if there is any directive before the anchor's id in restCommand *)
      let rec findIfDirectiveBeforeAnchor tokens id =
        match tokens with
        | [] -> false (* should not happen*)
        | token::rest ->
          (match token with
          | FeaturePath.Token.Directive _ -> true
          | FeaturePath.Token.Anchor a -> if a = id then false else findIfDirectiveBeforeAnchor rest id
          | _ -> findIfDirectiveBeforeAnchor rest id)
      in
      not (findIfDirectiveBeforeAnchor restCommand id)

let getApplicableAnchors 
    (anchors: Feature.OptionAnchor.t StringMap.t)
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
      checkConstraint evaluatedCommand restCommand constraint_ id
    ) constraints
  )
  |> List.map (fun (id,_) -> id)