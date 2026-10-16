open Ocf
open Repr
open State

(* [ongoingState] where the argument [l] holds [value] *)
let withArg ({configuration; _} as ongoingState : ongoingState) l value =
  {ongoingState with configuration = {configuration with Configuration.args = StringMap.add l value configuration.args}}

(* [ongoingState] where the entry token [he] has been consumed, the entry continuing with [te] *)
let consumeEntry ({tokenStacks = stacks; _} as ongoingState : ongoingState) he te =
  {ongoingState with tokenStacks = {stacks with evaluatedEntry = stacks.evaluatedEntry @ [he]; restEntry = te}}

(* [ongoingState] where the command token [hc] has been evaluated, the command continuing with [tc] *)
let nextCommand ({tokenStacks = stacks; _} as ongoingState : ongoingState) hc tc =
  {ongoingState with tokenStacks = {stacks with evaluatedCommand = stacks.evaluatedCommand @ [hc]; restCommand = tc}}

(* evaluates the argument location at the head of the command against the next entry token,
   or closes it when the entry has been fully consumed *)
let evalArgument
  (args : Feature.Argument.any StringMap.t)
  ({configuration; tokenStacks = stacks; _} as ongoingState : ongoingState)
  : ongoingState list =
  match stacks.restCommand with
  | [] | (FeaturePath.Token.Directive _ | FeaturePath.Token.Anchor _) :: _ ->
    invalid_arg "Argument.evalArgument: the command does not continue with an argument location"
  | (FeaturePath.Token.ArgumentLocation l as hc) :: tc ->
  let (Feature.Argument.Any arg) = StringMap.find l args in
  (* the argument is left empty, the command continues *)
  let skip value = nextCommand (withArg ongoingState l value) hc tc in
  (* a list argument already holding values was kept open when its values were consumed:
     the alternative that closes it has been generated at that time *)
  let isNew = not (StringMap.mem l configuration.args) in
  (* evaluates the entry token [he] against a list argument holding between [min] and [max] values
     (max = None means unbounded) *)
  let evalList (type a) min max (encoding : a TokenEncoding.tokenEncoding) he te =
    let reachesMax n = match max with Some m -> n >= m | None -> false in
    let listTypeformat = Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding) in
    let values : a list = match StringMap.find_opt l configuration.args with
      | None -> []
      | Some (Typeformat.Value (tf, values)) ->
        match Typeformat.equal_type listTypeformat tf with
        | Some Type.Equal -> values
        | None -> failwith "This should not happen, argument location (multiple strings) should always be associated with a list of its encoding in the configuration"
    in
    let n = List.length values in
    if reachesMax n then
      failwith (Format.asprintf "Too many arguments for %s, maximum is %d" l (Stdlib.Option.get max))
    else
    (* the argument can be left empty only before any value has been consumed *)
    let skipped = if min = 0 && isNew then [skip (Typeformat.Value (listTypeformat, []))] else [] in
    match Typeformat.value_of_token encoding he with
    | None -> if skipped = [] then [{ongoingState with case = Unmatched hc}] else skipped
    | Some value ->
      let consumed = consumeEntry (withArg ongoingState l (Typeformat.Value (listTypeformat, values @ [value]))) he te in
      (* close the list: only once it holds enough values *)
      let closed = if n + 1 >= min then [nextCommand consumed hc tc] else [] in
      (* keep the list open for more values: only while below the maximum *)
      let kept = if reachesMax (n + 1) then [] else [consumed] in
      skipped @ closed @ kept
  in
  match stacks.restEntry, arg.encoding with
  | [], Feature.Argument.Optional encoding when isNew ->
    [skip (Typeformat.Value (Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding encoding), None))]
  | [], Feature.Argument.Over (0, encoding) when isNew ->
    [skip (Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), []))]
  | [], Feature.Argument.Between (0, _, encoding) when isNew ->
    [skip (Typeformat.Value (Typeformat.TList (Typeformat.typeformat_of_tokenEncoding encoding), []))]
  | [], _ -> [{ongoingState with case = Incomplete hc}]
  | he :: te, Feature.Argument.Single encoding ->
    begin match Typeformat.value_of_token encoding he with
    | Some value -> [nextCommand (consumeEntry (withArg ongoingState l (Typeformat.Value (Typeformat.typeformat_of_tokenEncoding encoding, value))) he te) hc tc]
    | None -> [{ongoingState with case = Unmatched hc}]
    end
  | he :: te, Feature.Argument.Optional encoding ->
    let typeformat = Typeformat.TOption (Typeformat.typeformat_of_tokenEncoding encoding) in
    let skipped = skip (Typeformat.Value (typeformat, None)) in
    begin match Typeformat.value_of_token encoding he with
    | Some value -> [nextCommand (consumeEntry (withArg ongoingState l (Typeformat.Value (typeformat, Some value))) he te) hc tc; skipped]
    | None -> [skipped]
    end
  | he :: te, Feature.Argument.Over (min, encoding) -> evalList min None encoding he te
  | he :: te, Feature.Argument.Between (min, max, encoding) -> evalList min (Some max) encoding he te
