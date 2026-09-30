type stringFormat = | QuotedString | SingleWord | FilePath
[@@deriving yojson, show]

type 'a tokenEncoding =
  | SimpleString : stringFormat option -> string tokenEncoding
  | Integer : int tokenEncoding
  | Enum : string list -> string tokenEncoding

let decodeToken : type a. a tokenEncoding -> string -> a = function
  | SimpleString _ -> fun s -> s
  | Integer -> fun s -> int_of_string s
  | Enum _ -> fun s -> s

let encodeToken : type a. a tokenEncoding -> a -> string = function
  | SimpleString _ -> fun s -> s
  | Integer -> fun i -> string_of_int i
  | Enum _ -> fun s -> s
