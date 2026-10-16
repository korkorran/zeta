open Ocf
open Repr
open State

let getOptionsFromAnchor (feature : Feature.feature) (availableAnchor : string) : (string * Feature.CommandOption.any) list =
  StringMap.bindings feature.options
  |> List.filter (fun (_, Feature.CommandOption.Any opt) -> opt.location = availableAnchor)

(* the typeformat of the value carried by one occurrence of an option *)
let typeformat_of_written written =
  Typeformat.typeformat_of_tokenEncoding (Feature.CommandOption.valueEncoding written)

(* reads one occurrence of an option at the head of the entry: its value, the consumed tokens and the rest of the entry *)
let readOccurrence : type a. a Feature.CommandOption.written -> string list -> (a * string list * string list) option = fun written restEntry ->
  match written, restEntry with
  | Feature.CommandOption.SingleToken encoding, he :: te ->
    Typeformat.value_of_token encoding he |> Stdlib.Option.map (fun value -> value, [he], te)
  | Feature.CommandOption.ValueNextToken (keyEncoding, encoding), h1 :: h2 :: te ->
    begin match Typeformat.value_of_token keyEncoding h1, Typeformat.value_of_token encoding h2 with
    | Some (), Some value -> Some (value, [h1; h2], te)
    | _ -> None
    end
  | _ -> None

(* appends [value] to the values of a repeatable option already in the configuration *)
let appendValue : type a. a Typeformat.typeformat -> Typeformat.value option -> a -> Typeformat.value = fun typeformat previous value ->
  let listTypeformat = Typeformat.TList typeformat in
  match previous with
  | None -> Typeformat.Value (listTypeformat, [value])
  | Some (Typeformat.Value (tf, values)) ->
    match Typeformat.equal_type listTypeformat tf with
    | Some Type.Equal -> Typeformat.Value (listTypeformat, values @ [value])
    | None -> failwith "This should not happen, a repeatable option should always be associated with a list of its encoding in the configuration"

(* evaluates the option [id] against the head of the entry, None if the entry does not start with it *)
let evalOption
  (id, Feature.CommandOption.Any option : string * Feature.CommandOption.any)
  ({configuration; tokenStacks = stacks; _} as ongoingState : ongoingState)
  : ongoingState option =
  (* [ongoingState] where the option holds [value], the entry tokens [tokens] being consumed *)
  let withOption value tokens restEntry =
    {ongoingState with
      configuration = {configuration with Configuration.options = StringMap.add id value configuration.options};
      tokenStacks = {stacks with evaluatedEntry = stacks.evaluatedEntry @ tokens; restEntry}}
  in
  match option.encoding with
  (* the last occurrence wins *)
  | Feature.CommandOption.Once written ->
    readOccurrence written stacks.restEntry |> Stdlib.Option.map (fun (value, tokens, rest) ->
      withOption (Typeformat.Value (Typeformat.TOption (typeformat_of_written written), Some value)) tokens rest)
  | Feature.CommandOption.Repeatable written ->
    readOccurrence written stacks.restEntry |> Stdlib.Option.map (fun (value, tokens, rest) ->
      withOption (appendValue (typeformat_of_written written) (StringMap.find_opt id configuration.options) value) tokens rest)
