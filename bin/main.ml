open Opencommand.Command

let () = print_endline "Hello, World!"

let mv = Native_registry.Mv.mv

let () = print_endline (Printf.sprintf "Registered command: mv - %s"
                  (Opencommand.Utils.StringMap.find "mv [?options] [<source>] <destination>" mv).description)

let json = Yojson.Safe.to_string (Opencommand.Command.commandPaths_to_yojson mv)

let () = print_endline (Printf.sprintf "Serialized command: %s" json)

