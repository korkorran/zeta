open Opencommand.Command

let () = print_endline "Hello, World!"

let mv = Native_registry.Mv.mv

let () = print_endline (Printf.sprintf "Registered command: mv - %s"
                  (Opencommand.Utils.StringMap.find "mv [?options] [<source>] <destination>" mv.paths).description)

let json = Yojson.Safe.to_string (Opencommand.Command.commandPaths_to_yojson mv.paths)

let () = print_endline (Printf.sprintf "Serialized command: %s" json)

let string = {|{"mv [?options] [<source>] <destination>":{"options":{"force":{"encoding":["SingleString",["-f"]],"description":"Force move","location":"options"},"interactive":{"encoding":["SingleString",["-i"]],"description":"Prompt before move","location":"options"},"verbose":{"encoding":["SingleString",["-v"]],"description":"Verbose output","location":"options"}},"optionsLocations":{"options":{"from":1,"to_":10}},"args":{"destination":{"encoding":["SingleString"],"description":"Destination file/directory"},"source":{"encoding":["MultipleStrings",[1,3]],"description":"Source file/directory"}},"description":"Move or rename a file, a directory, or a symlink"}}|}

let mv_parsed = Opencommand.Command.commandPaths_of_yojson (Yojson.Safe.from_string string)

let () = match mv_parsed with
  | Ok mv_parsed -> print_endline (Printf.sprintf "Parsed command: %s" (Opencommand.Utils.StringMap.find "mv [?options] [<source>] <destination>" mv_parsed).description)
  | Error e -> print_endline (Printf.sprintf "Error parsing command: %s" e)
