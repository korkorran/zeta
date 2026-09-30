module CommandOption = struct

  type encoding =
  | SingleString : string list -> encoding
  | ValueSuffix : string list * ('a TokenEncoding.tokenEncoding) -> encoding

  type t = {
    encoding: encoding;
    description: string;
    location: string;
  }
end


module Argument = struct
  type encoding = 
    | Single : 'a TokenEncoding.tokenEncoding -> encoding
    | ArgList : ('a TokenEncoding.tokenEncoding*int*int) -> encoding  (* min, max *)

  type t = {
    encoding: encoding;
    description: string;
  }
end

module OptionAnchor = struct
  type constraint_ = ExactLocation | AfterLastDirective

  type t = constraint_ list
end




type feature = {
  path: FeaturePath.EncodedPath.t;
  options: CommandOption.t StringMap.t;
  anchors: OptionAnchor.t StringMap.t;
  args: Argument.t StringMap.t;
  description: string;
}

type featureMap = feature StringMap.t

let addFeature path (feature:feature) cmdFeatures =
  StringMap.add path feature cmdFeatures