open Opencommand.Command

let () = print_endline "Hello, World!"

let git = Native_registry.Git.git

let () = print_endline (Printf.sprintf "Registered command: git - %s"
                  (Opencommand.Utils.StringMap.find "git" git).description)

(* let c = parseEncodedCommand git

let () = ignore c *)
