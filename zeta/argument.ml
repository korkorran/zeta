open Ocf
open Repr
open State

(* (min, max) number of entry tokens accepted by an argument, max = None means unbounded *)
let argumentBounds : type a. a Feature.Argument.encoding -> int * int option = function
  | Feature.Argument.Single _ -> (1, Some 1)
  | Feature.Argument.Optional _ -> (0, Some 1)
  | Feature.Argument.Over (min, _) -> (min, None)
  | Feature.Argument.Between (min, max, _) -> (min, Some max)

(* evaluates the next entry token against a list argument location [hc] (named [l]),
   holding between [min] and [max] values (max = None means unbounded) *)
let evalList (type a)
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
  match Typeformat.value_of_token encoding he with
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

(* evaluates the argument location [hc] (named [l]) against the next entry token,
   or closes it when the entry has been fully consumed *)
let evalArgument
  (ongoingState : ongoingState)
  (hc : FeaturePath.Token.t)
  (l : string)
  (Feature.Argument.Any arg : Feature.Argument.any)
  : ongoingState list =
  let currentConfig = ongoingState.configuration in
  let currentStacks = ongoingState.evaluationStacks in
  let withArg value = {Configuration.options=currentConfig.options; args=StringMap.add l value currentConfig.args} in
  let skipArgument = {ongoingState with evaluationStacks = {
    evaluatedEntry = currentStacks.evaluatedEntry;
    restEntry = currentStacks.restEntry;
    evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
    restCommand = List.tl currentStacks.restCommand;
  }} in
  match currentStacks.restEntry with
  | [] ->
    let emptyValue () = match arg.encoding with
      | Feature.Argument.Single _ -> assert false (* a Single argument requires one value, see argumentBounds *)
      | Feature.Argument.Optional encoding -> Typeformat.Value (Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding encoding), None)
      | Feature.Argument.Over (_, encoding) -> Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), [])
      | Feature.Argument.Between (_, _, encoding) -> Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), [])
    in
    (* a list argument already holding values was kept open when its values were consumed:
       the alternative that closes it has been generated at that time *)
    if fst (argumentBounds arg.encoding) = 0 && not (StringMap.mem l currentConfig.args) then
      [{skipArgument with configuration = withArg (emptyValue ())}]
    else
      [{ongoingState with case = Incomplete hc}]
  | he :: te ->
    let consumed value = {ongoingState with configuration = withArg value; evaluationStacks = {
      evaluatedEntry = currentStacks.evaluatedEntry @ [he];
      restEntry = te;
      evaluatedCommand = currentStacks.evaluatedCommand @ [hc];
      restCommand = List.tl currentStacks.restCommand;
    }} in
    match arg.encoding with
    | Feature.Argument.Single encoding ->
      let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
      (match Typeformat.value_of_token encoding he with
      | None -> [{ongoingState with case = Unmatched hc}]
      | Some value -> [consumed (Typeformat.Value (typeformat, value))])
    | Feature.Argument.Optional encoding ->
      let typeformat = Typeformat.typeformat_of_tokenEncoding encoding in
      let skipped = {skipArgument with configuration = withArg (Typeformat.Value (Typeformat.TOption typeformat, None))} in
      (match Typeformat.value_of_token encoding he with
      | None -> [skipped]
      | Some value -> [consumed (Typeformat.Value (Typeformat.TOption typeformat, Some value)); skipped])
    | Feature.Argument.Over (min, encoding) ->
      evalList ongoingState hc l skipArgument min None encoding
    | Feature.Argument.Between (min, max, encoding) ->
      evalList ongoingState hc l skipArgument min (Some max) encoding
