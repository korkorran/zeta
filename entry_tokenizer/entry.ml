type value = string list
 [@@deriving show]

let output_value out_channel = pp_value (Format.formatter_of_out_channel out_channel)