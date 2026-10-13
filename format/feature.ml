module CommandOption = struct

  (* how one occurrence of the option is written *)
  type 'a written =
  | SingleToken : 'a TokenEncoding.tokenEncoding -> 'a written
  | ValueNextToken : unit TokenEncoding.tokenEncoding * ('a TokenEncoding.tokenEncoding) -> 'a written

  (* how many times the option may appear *)
  type 'a encoding =
  | Once : 'a written -> 'a option encoding
  | Repeatable : 'a written -> 'a list encoding

  (* the encoding of the value carried by one occurrence of the option *)
  let valueEncoding : type a. a written -> a TokenEncoding.tokenEncoding = function
  | SingleToken encoding -> encoding
  | ValueNextToken (_, encoding) -> encoding

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