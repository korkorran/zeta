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
  type 'a encoding = 
    | Optional : 'a TokenEncoding.tokenEncoding -> 'a option encoding
    | Single : 'a TokenEncoding.tokenEncoding -> 'a encoding
    | Over : int * 'a TokenEncoding.tokenEncoding -> 'a list encoding
    | Between : int * int * 'a TokenEncoding.tokenEncoding -> 'a list encoding

  type 'a t = {
    encoding: 'a encoding;
    description: string;
  }

  type any = Any : 'a t -> any
end

module OptionAnchor = struct
  type constraint_ = ExactLocation | AfterLastDirective

  type t = constraint_ list
end




type feature = {
  path: FeaturePath.EncodedPath.t;
  options: CommandOption.t StringMap.t;
  anchors: OptionAnchor.t StringMap.t;
  args: Argument.any StringMap.t;
  description: string;
}

type featureMap = feature StringMap.t

let addFeature path (feature:feature) cmdFeatures =
  StringMap.add path feature cmdFeatures