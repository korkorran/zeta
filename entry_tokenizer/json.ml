type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]

let output_value out_channel = Yojson.Basic.pp (Format.formatter_of_out_channel out_channel)