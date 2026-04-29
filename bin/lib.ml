type 'a tokenEncoding =
  | SimpleString : string tokenEncoding

let decodeToken : type a. a tokenEncoding -> string -> a = function
  | SimpleString -> fun s -> s

let encodeToken : type a. a tokenEncoding -> a -> string = function
  | SimpleString -> fun s -> s

type 't optionEncoding =
  | SingleString : string list -> unit optionEncoding
  | ValueSuffix : string list * ('t tokenEncoding) -> 't optionEncoding
  [@@deriving yojson]

type 'a commandOption = {
    encoding: 'a optionEncoding;
    description: string;
    location: string;
  }

type 't argumentEncoding =
  | Single : ('t tokenEncoding) -> 't argumentEncoding
  | StringList : (('t tokenEncoding) * int * int) -> 't list argumentEncoding
  [@@deriving yojson]

type 'a commandArg = {
    encoding: 'a argumentEncoding;
    description: string;
}

let emptyCommandOptions = ()
let emptyCommandArgs = ()

type ('options, 'args) feature = {
    path : string;
    options : 'options;
    args : 'args;
    description : string;
  }