module CommandOption = struct

  type 'a encoding =
  | SingleString : string list -> unit option encoding
  | ValueSuffix : string list * ('a TokenEncoding.tokenEncoding) -> 'a encoding

  type 'a t = {
    encoding: 'a encoding;
    description: string;
    location: string;
  }

  type any = Any : 'a t -> any
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
  options: CommandOption.any StringMap.t;
  anchors: OptionAnchor.t StringMap.t;
  args: Argument.any StringMap.t;
  description: string;
}

type featureMap = feature StringMap.t

let addFeature path (feature:feature) cmdFeatures =
  StringMap.add path feature cmdFeatures