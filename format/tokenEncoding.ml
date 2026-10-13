type stringFormat = | QuotedString | SingleWord | FilePath
[@@deriving yojson, show]

type 'a tokenEncoding =
  | FixedString : string list -> unit tokenEncoding
  | FormattedString : stringFormat option -> string tokenEncoding
  | Integer : int tokenEncoding
  | Enum : string list -> string tokenEncoding

