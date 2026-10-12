open Ocf
open Repr


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
  configuration : Configuration.configuration;
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
    